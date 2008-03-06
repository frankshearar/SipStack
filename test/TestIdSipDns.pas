{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipDns;

interface

uses
  IdSipDns, TestFramework;

type
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
    procedure TestAsString;
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestResourceType;
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
    procedure TestAsString;
    procedure TestClear;
    procedure TestCopy;
    procedure TestIsEmpty;
    procedure TestSort;
  end;

  TestTIdDomainNameAliasRecord = class(TTestCase)
  private
    Alias:         String;
    CanonicalName: String;
    Rec:           TIdDomainNameAliasRecord;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestResourceType;
  end;

  TestTIdDomainNameAliasRecords = class(TTestCase)
  private
    List: TIdDomainNameAliasRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddWithParameters;
    procedure TestAsString;
    procedure TestClear;
    procedure TestCopy;
    procedure TestIsEmpty;
    procedure TestSort;
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
    procedure TestAsString;
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestIsSecureService;
    procedure TestResourceType;
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
    procedure TestAsString;
    procedure TestClear;
    procedure TestDelete;
    procedure TestIsEmpty;
    procedure TestRecordFor;
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
    procedure TestAsString;
    procedure TestCopy;
    procedure TestInstantiation;
    procedure TestQueryName;
    procedure TestResourceType;
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
    procedure TestAddServiceRecords;
    procedure TestAsString;
    procedure TestClear;
    procedure TestLast;
    procedure TestIsEmpty;
  end;

  TestTIdResourceRecords = class(TTestCase)
  private
    List: TIdResourceRecords;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddARecord;
    procedure TestAddAAAARecord;
    procedure TestAddCNAMERecord;
    procedure TestAddNAPTRRecord;
    procedure TestAddSRVRecord;
    procedure TestAsString;
    procedure TestCollectAliases;
    procedure TestCollectNamePointerRecords;
    procedure TestCollectNameRecords;
    procedure TestCollectServiceRecords;
    procedure TestContainsType;
  end;

implementation

uses
  Classes, IdSipMessage, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipDns unit tests');
  Result.AddTest(TestTIdDomainNameRecord.Suite);
  Result.AddTest(TestTIdDomainNameRecords.Suite);
  Result.AddTest(TestTIdDomainNameAliasRecord.Suite);
  Result.AddTest(TestTIdDomainNameAliasRecords.Suite);
  Result.AddTest(TestTIdNaptrRecord.Suite);
  Result.AddTest(TestTIdNaptrRecords.Suite);
  Result.AddTest(TestTIdSrvRecord.Suite);
  Result.AddTest(TestTIdSrvRecords.Suite);
  Result.AddTest(TestTIdResourceRecords.Suite);
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

procedure TestTIdDomainNameRecord.TestAsString;
begin
  CheckEquals(Format('%s %s %s', [Self.Rec.Domain, Self.Rec.ResourceType, Self.Rec.IPAddress]),
              Self.Rec.AsString,
              'AsString');
end;

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

procedure TestTIdDomainNameRecord.TestResourceType;
var
  NewRec: TIdDomainNameRecord;
begin
  NewRec := TIdDomainNameRecord.Create(DnsARecord, 'foo', '127.0.0.1');
  try
    CheckEquals(DnsARecord, NewRec.ResourceType, DnsARecord);
  finally
    NewRec.Free;
  end;

  NewRec := TIdDomainNameRecord.Create(DnsAAAARecord, 'foo', '::1');
  try
    CheckEquals(DnsAAAARecord, NewRec.ResourceType, DnsAAAARecord);
  finally
    NewRec.Free;
  end;
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

procedure TestTIdDomainNameRecords.TestAsString;
begin
  Self.List.Add(DnsARecord,    'foo.bar', '127.0.0.1');
  Self.List.Add(DnsAAAARecord, 'foo.bar', '2002:dead:f00d::1');

  CheckEquals(Self.List[0].AsString + CRLF + Self.List[1].AsString,
              Self.List.AsString,
              'AsString');
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

procedure TestTIdDomainNameRecords.TestSort;
begin
  Self.List.Add(DnsARecord,    'foo', '127.0.0.1');
  Self.List.Add(DnsAAAARecord, 'foo', '::1');
  Self.List.Add(DnsARecord,    'foo', '192.168.0.1');
  Self.List.Add(DnsAAAARecord, 'foo', '::1:1');
  Self.List.Add(DnsAAAARecord, 'bar', '127.0.0.1');

  Self.List.Sort;

  CheckEquals('bar',         Self.List[0].Domain,     '1st record');

  CheckEquals('foo',         Self.List[1].Domain,     '2nd record (domain)');
  CheckEquals(DnsAAAARecord, Self.List[1].RecordType, '2nd record (record type)');
  CheckEquals('::1',         Self.List[1].IPAddress,  '3rd record (IP address)');

  CheckEquals('foo',         Self.List[2].Domain,     '3rd record (domain)');
  CheckEquals(DnsAAAARecord, Self.List[2].RecordType, '3rd record (record type)');
  CheckEquals('::1:1',       Self.List[2].IPAddress,  '2nd record (IP address)');

  CheckEquals('foo',         Self.List[3].Domain,     '4th record (domain)');
  CheckEquals(DnsARecord,    Self.List[3].RecordType, '4th record (record type)');
  CheckEquals('127.0.0.1',   Self.List[3].IPAddress,  '5th record (IP address)');

  CheckEquals('foo',         Self.List[4].Domain,     '5th record (domain)');
  CheckEquals(DnsARecord,    Self.List[4].RecordType, '5th record (record type)');
  CheckEquals('192.168.0.1', Self.List[4].IPAddress,  '4th record (IP address)');
end;

//******************************************************************************
//* TestTIdDomainNameAliasRecord                                               *
//******************************************************************************
//* TestTIdDomainNameAliasRecord Public methods ********************************

procedure TestTIdDomainNameAliasRecord.SetUp;
begin
  inherited SetUp;

  Self.Alias         := 'alias';
  Self.CanonicalName := 'canonical.name';

  Rec := TIdDomainNameAliasRecord.Create(Self.CanonicalName, Self.Alias);
end;

procedure TestTIdDomainNameAliasRecord.TearDown;
begin
  Self.Rec.Free;

  inherited TearDown;
end;

//* TestTIdDomainNameAliasRecord Public methods ********************************

procedure TestTIdDomainNameAliasRecord.TestAsString;
begin
  CheckEquals(Format('%s %s %s', [Self.Rec.Alias, Self.Rec.ResourceType, Self.Rec.CanonicalName]),
              Self.Rec.AsString,
              'AsString');
end;

procedure TestTIdDomainNameAliasRecord.TestCopy;
var
  Copy: TIdDomainNameAliasRecord;
begin
  Copy := Self.Rec.Copy;
  try
    CheckEquals(Self.Rec.Alias,         Copy.Alias,         'Alias');
    CheckEquals(Self.Rec.CanonicalName, Copy.CanonicalName, 'CanonicalName');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdDomainNameAliasRecord.TestInstantiation;
begin
  CheckEquals(Self.Alias,         Self.Rec.Alias,         'Alias');
  CheckEquals(Self.CanonicalName, Self.Rec.CanonicalName, 'CanonicalName');
end;

procedure TestTIdDomainNameAliasRecord.TestResourceType;
begin
  CheckEquals(DnsCNAMERecord, Self.Rec.ResourceType, DnsCNAMERecord);
end;

//******************************************************************************
//* TestTIdDomainNameAliasRecords                                              *
//******************************************************************************
//* TestTIdDomainNameAliasRecords Public methods *******************************

procedure TestTIdDomainNameAliasRecords.SetUp;
begin
  inherited SetUp;

  Self.List := TIdDomainNameAliasRecords.Create;
end;

procedure TestTIdDomainNameAliasRecords.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdDomainNameAliasRecords Published methods ****************************

procedure TestTIdDomainNameAliasRecords.TestAdd;
var
  NewRec: TIdDomainNameAliasRecord;
begin
  CheckEquals(0, Self.List.Count, 'Empty list');

  NewRec := TIdDomainNameAliasRecord.Create('', '');
  try
    Self.List.Add(NewRec);
    CheckEquals(1, Self.List.Count, 'Non-empty list');
  finally
    NewRec.Free;
  end;
end;

procedure TestTIdDomainNameAliasRecords.TestAddWithParameters;
const
  Alias         = 'sipproxy.leo-ix.net';
  CanonicalName = 'gw1.leo-ix.net';
begin
  Self.List.Add(CanonicalName, Alias);

  CheckEquals(Alias,         Self.List[0].Alias,         'Alias');
  CheckEquals(CanonicalName, Self.List[0].CanonicalName, 'CanonicalName');
end;

procedure TestTIdDomainNameAliasRecords.TestAsString;
begin
  Self.List.Add('foo.bar', 'foo1.bar');
  Self.List.Add('foo.bar', 'foo2.bar');

  CheckEquals(Self.List[0].AsString + CRLF + Self.List[1].AsString,
              Self.List.AsString,
              'AsString');
end;

procedure TestTIdDomainNameAliasRecords.TestClear;
var
  NewRec: TIdDomainNameAliasRecord;
begin
  NewRec := TIdDomainNameAliasRecord.Create('', '');
  try
    Self.List.Add(NewRec);
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Self.List.Clear;

  CheckEquals(0, Self.List.Count, 'Cleared list');
end;

procedure TestTIdDomainNameAliasRecords.TestCopy;
var
  I:      Integer;
  NewRec: TIdDomainNameAliasRecord;
  NewSet: TIdDomainNameAliasRecords;
begin
  NewRec := TIdDomainNameAliasRecord.Create('foo.bar', 'quaax.bar');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewRec := TIdDomainNameAliasRecord.Create('foo.bar', 'baz.bar');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  NewSet := Self.List.Copy;
  try
    for I := 0 to Min(NewSet.Count, Self.List.Count) - 1 do begin
      CheckEquals(Self.List[I].Alias,
                  NewSet[I].Alias,
                  'Alias at index ' + IntToStr(I));
      CheckEquals(Self.List[I].CanonicalName,
                  NewSet[I].CanonicalName,
                  'CanonicalName at index ' + IntToStr(I));

    end;

    CheckEquals(Self.List.Count,
                NewSet.Count,
                'Record count');
  finally
    NewSet.Free;
  end;
end;

procedure TestTIdDomainNameAliasRecords.TestIsEmpty;
var
  NewRec: TIdDomainNameAliasRecord;
begin
  Check(Self.List.IsEmpty, 'Empty list');

  NewRec := TIdDomainNameAliasRecord.Create('', '');
  try
    Self.List.Add(NewRec);
  finally
    NewRec.Free;
  end;

  Check(not Self.List.IsEmpty, 'After Add');

  Self.List.Clear;

  Check(Self.List.IsEmpty, 'After clear');
end;

procedure TestTIdDomainNameAliasRecords.TestSort;
var
  I: Integer;
begin
  Self.List.Add('quaax', '3');
  Self.List.Add('quaax', '4');
  Self.List.Add('foo', '1');
  Self.List.Add('foo', '2');

  Self.List.Sort;

  for I := 0 to Self.List.Count - 1 do
    CheckEquals(IntToStr(I + 1), Self.List[I].Alias, 'Record #' + IntToStr(I + 1));
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

procedure TestTIdNaptrRecord.TestAsString;
begin
  CheckEquals(Format('%s %s %d %d %s %s %s %s', [Self.Rec.Key, Self.Rec.ResourceType, Self.Rec.Order, Self.Rec.Preference, Self.Rec.Flags, Self.Rec.Service, Self.Rec.Regex, Self.Rec.Service]),
              Self.Rec.AsString,
              'AsString');
end;

procedure TestTIdNaptrRecord.TestCopy;
var
  Copy: TIdNaptrRecord;
  Srv:  TIdSrvRecord;
begin
  Srv := TIdSrvRecord.Create('leo-ix.net', '_sip._tcp', 2, 0, 5060, 'sip-proxy');
  try
    Self.Rec.ServiceRecords.Add(Srv);

    Copy := Self.Rec.Copy;
    try
      CheckEquals(Self.Rec.Flags,      Copy.Flags,      'Flags');
      CheckEquals(Self.Rec.Key,        Copy.Key,        'Key');
      CheckEquals(Self.Rec.Order,      Copy.Order,      'Order');
      CheckEquals(Self.Rec.Preference, Copy.Preference, 'Preference');
      CheckEquals(Self.Rec.Regex,      Copy.Regex,      'Regex');
      CheckEquals(Self.Rec.Service,    Copy.Service,    'Service');
      CheckEquals(Self.Rec.Value,      Copy.Value,      'Value');

      CheckEquals(Self.Rec.ServiceRecords.Count,
                  Copy.ServiceRecords.Count,
                  'Service records not copied');
      CheckEquals(Self.Rec.ServiceRecords[0].Target,
                  Copy.ServiceRecords[0].Target,
                  'SRV records not copied correctly');
    finally
      Copy.Free;
    end;
  finally
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

procedure TestTIdNaptrRecord.TestResourceType;
begin
  CheckEquals(DnsNAPTRRecord, Self.Rec.ResourceType, DnsNAPTRRecord);
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

procedure TestTIdNaptrRecords.TestAsString;
begin
  Self.List.Add('foo', 0, 0, NaptrDefaultFlags, NaptrTlsService,  '', SrvTlsPrefix + '.foo');
  Self.List.Add('foo', 1, 0, NaptrDefaultFlags, NaptrTcpService,  '', SrvTcpPrefix + '.foo');

  CheckEquals(Self.List[0].AsString + CRLF + Self.List[1].AsString,
              Self.List.AsString,
              'AsString');
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

procedure TestTIdNaptrRecords.TestRecordFor;
const
  SipOverTcpTarget   = '_sip._tcp.bar';
  WeirdServiceTarget = 'foo.bar';
var
  N: TIdNaptrRecord;
begin
  Self.List.Add('foo', 30, 10, 's', 'SIP+D2U',  '', '_sip._udp.foo');
  Self.List.Add('bar', 10, 10, 's', 'http+foo', '', WeirdServiceTarget);
  Self.List.Add('bar', 20, 10, 's', 'SIP+D2T',  '', SipOverTcpTarget);
  Self.List.Add('bar', 10, 10, 's', 'SIPS+D2T', '', '_sips._tcp.bar');
  Self.List.Add('bar', 30, 10, 's', 'SIP+D2U',  '', '_sip._udp.bar');

  CheckNull(Self.List.RecordFor(''), 'The empty string');
  CheckNull(Self.List.RecordFor('_sip._udp.quaax'), 'A valid target that''s not present');

  N := Self.List.RecordFor(SipOverTcpTarget);
  CheckNotNull(N, 'An extant record');
  CheckEquals(SipOverTcpTarget, N.Value, 'Wrong record returned');

  N := Self.List.RecordFor(WeirdServiceTarget);
  CheckNotNull(N, 'An extant record for a weird service');
  CheckEquals(WeirdServiceTarget, N.Value, 'Wrong record returned for a weird service');
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
  Self.Port        := DefaultSipsPort;
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

procedure TestTIdSrvRecord.TestAsString;
begin
  CheckEquals(Format('%s%s %s %d %d %d %s', [Self.Rec.Service, Self.Rec.Domain, Self.Rec.ResourceType, Self.Rec.Priority, Self.Rec.Weight, Self.Rec.Port, Self.Rec.Target]),
              Self.Rec.AsString,
              'AsString');
end;

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

procedure TestTIdSrvRecord.TestResourceType;
begin
  CheckEquals(DnsSRVRecord, Self.Rec.ResourceType, DnsSRVRecord);
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
  Port     = DefaultSipsPort;
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

procedure TestTIdSrvRecords.TestAddServiceRecords;
var
  NewList: TIdSrvRecords;
  I:       Integer;
begin
  NewList := TIdSrvRecords.Create;
  try
    NewList.Add('leo-ix.net', '_sips._tcp', 1, 2, 5061, 'paranoid');
    NewList.Add('leo-ix.net', '_sip._tcp',  2, 0, 5060, 'sip-proxy');
    NewList.Add('leo-ix.net', '_sip._udp',  2, 0, 5060, 'sip-proxy');

    Self.List.AddServiceRecords(NewList);

    CheckEquals(NewList.Count, Self.List.Count, 'Unexpected number of records');

    for I := 0 to Self.List.Count - 1 do
      CheckEquals(NewList[I].QueryName,
                  Self.List[I].QueryName,
                  'RR #' + IntToStr(I) + ' not copied correctly');
  finally
    NewList.Free;
  end;
end;

procedure TestTIdSrvRecords.TestAsString;
begin
  Self.List.Add('leo-ix.net', '_sips._tcp', 1, 2, 5061, 'paranoid');
  Self.List.Add('leo-ix.net', '_sip._tcp',  2, 0, 5060, 'sip-proxy');

  CheckEquals(Self.List[0].AsString + CRLF + Self.List[1].AsString,
              Self.List.AsString,
              'AsString');
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

//******************************************************************************
//* TestTIdResourceRecords                                                     *
//******************************************************************************
//* TestTIdResourceRecords Public methods **************************************

procedure TestTIdResourceRecords.SetUp;
begin
  inherited SetUp;

  Self.List := TIdResourceRecords.Create;
end;

procedure TestTIdResourceRecords.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdResourceRecords Published methods ***********************************

procedure TestTIdResourceRecords.TestAddARecord;
const
  Domain  = 'gw1.leo.ix.net';
  Address = '127.0.0.1';
var
  ARec: TIdDomainNameRecord;
begin
  Self.List.AddARecord(Domain, Address);

  Check(not Self.List.IsEmpty, 'No record added');

  CheckEquals(TIdDomainNameRecord,
              Self.List.LastRecord.ClassType,
              'Wrong type of record added');

  ARec := Self.List.LastRecord as TIdDomainNameRecord;
  CheckEquals(DnsARecord, ARec.RecordType, 'RecordType');
  CheckEquals(Domain,     ARec.Domain,     'Domain');
  CheckEquals(Address,    ARec.IPAddress,  'Address');
end;

procedure TestTIdResourceRecords.TestAddAAAARecord;
const
  Domain  = 'gw1.leo.ix.net';
  Address = '127.0.0.1';
var
  AAAARec: TIdDomainNameRecord;
begin
  Self.List.AddAAAARecord(Domain, Address);

  Check(not Self.List.IsEmpty, 'No record added');

  CheckEquals(TIdDomainNameRecord,
              Self.List.LastRecord.ClassType,
              'Wrong type of record added');

  AAAARec := Self.List.LastRecord as TIdDomainNameRecord;
  CheckEquals(DnsAAAARecord, AAAARec.RecordType, 'RecordType');
  CheckEquals(Domain,        AAAARec.Domain,     'Domain');
  CheckEquals(Address,       AAAARec.IPAddress,  'Address');
end;

procedure TestTIdResourceRecords.TestAddCNAMERecord;
const
  Alias         = '127.0.0.1';
  CanonicalName = 'gw1.leo.ix.net';
var
  CNAMERec: TIdDomainNameAliasRecord;
begin
  Self.List.AddCNAMERecord(CanonicalName, Alias);

  Check(not Self.List.IsEmpty, 'No record added');

  CheckEquals(TIdDomainNameAliasRecord,
              Self.List.LastRecord.ClassType,
              'Wrong type of record added');

  CNAMERec := Self.List.LastRecord as TIdDomainNameAliasRecord;
  CheckEquals(CanonicalName, CNAMERec.CanonicalName, 'CanonicalName');
  CheckEquals(Alias,         CNAMERec.Alias,         'Alias');
end;

procedure TestTIdResourceRecords.TestAddNAPTRRecord;
const
  Flags          = 'fakeflags';
  Key            = 'fakekey';
  Order          = 42;
  Preference     = 13;
  Regex          = 'fakeregex';
  Service        = 'fakeservice';
  Value          = 'fakereplacement';
var
  NAPTRRec: TIdNaptrRecord;
begin
  Self.List.AddNAPTRRecord(Key, Order, Preference, Flags, Service, Regex, Value);

  Check(not Self.List.IsEmpty, 'No record added');

  CheckEquals(TIdNaptrRecord,
              Self.List.LastRecord.ClassType,
              'Wrong type of record added');

  NAPTRRec := Self.List.LastRecord as TIdNaptrRecord;
  CheckEquals(Flags,      NAPTRRec.Flags,      'Flags');
  CheckEquals(Key,        NAPTRRec.Key,        'Key');
  CheckEquals(Order,      NAPTRRec.Order,      'Order');
  CheckEquals(Preference, NAPTRRec.Preference, 'Preference');
  CheckEquals(Regex,      NAPTRRec.Regex,      'Regex');
  CheckEquals(Service,    NAPTRRec.Service,    'Service');
  CheckEquals(Value,      NAPTRRec.Value,      'Value');
end;

procedure TestTIdResourceRecords.TestAddSRVRecord;
const
  Domain   = 'fakedomain';
  Service  = 'fakeservice';
  Priority = 42;
  Weight   = 13;
  Port     = 666;
  Target   = 'faketarget';
var
  SRVRec: TIdSrvRecord;
begin
  Self.List.AddSRVRecord(Domain, Service, Priority, Weight, Port, Target);

  Check(not Self.List.IsEmpty, 'No record added');

  CheckEquals(TIdSrvRecord,
              Self.List.LastRecord.ClassType,
              'Wrong type of record added');

  SRVRec := Self.List.LastRecord as TIdSrvRecord;
  CheckEquals(Domain,   SRVRec.Domain,   'Domain');
  CheckEquals(Service,  SRVRec.Service,  'Service');
  CheckEquals(Priority, SRVRec.Priority, 'Priority');
  CheckEquals(Weight,   SRVRec.Weight,   'Weight');
  CheckEquals(Port,     SRVRec.Port,     'Port');
  CheckEquals(Target,   SRVRec.Target,   'Target');
end;

procedure TestTIdResourceRecords.TestAsString;
begin
  Self.List.AddARecord('foo', '127.0.0.1');
  Self.List.AddARecord('bar', '127.0.0.1');
  Self.List.AddSRVRecord('fakedomain', 'fakeservice', 42, 13, 666, 'faketarget');

  CheckEquals(Self.List[0].AsString + CRLF + Self.List[1].AsString + CRLF + Self.List[2].AsString,
              Self.List.AsString,
              'AsString');
end;

procedure TestTIdResourceRecords.TestCollectAliases;
const
  AliasOne         = 'baz1';
  CanonicalNameOne = 'baz';
  AliasTwo         = 'quaax1';
  CanonicalNameTwo = 'quaax';
var
  C: TIdDomainNameAliasRecords;
begin
  Self.List.AddARecord('foo', '127.0.0.1');
  Self.List.AddARecord('bar', '127.0.0.1');
  Self.List.AddCNAMERecord(CanonicalNameOne, AliasOne);
  Self.List.AddCNAMERecord(CanonicalNameTwo, AliasTwo);

  C := TIdDomainNameAliasRecords.Create;
  try
    Self.List.CollectAliases(C);

    CheckEquals(2, C.Count, 'Incorrect number of CNAMEs collected');
    CheckEquals(CanonicalNameOne, C[0].CanonicalName, '1st record');
    CheckEquals(CanonicalNameTwo, C[1].CanonicalName, '2nd record');
  finally
    C.Free;
  end;
end;

procedure TestTIdResourceRecords.TestCollectNamePointerRecords;
const
  KeyOne = 'one';
  KeyTwo = 'two';
var
  N: TIdNaptrRecords;
begin
  Self.List.AddARecord('foo', '127.0.0.1');
  Self.List.AddARecord('bar', '127.0.0.1');
  Self.List.AddNAPTRRecord(KeyOne, 0, 0, '', '', '', '');
  Self.List.AddNAPTRRecord(KeyTwo, 0, 0, '', '', '', '');

  N := TIdNaptrRecords.Create;
  try
    Self.List.CollectNamePointerRecords(N);

    CheckEquals(2, N.Count, 'Incorrect number of NAPTRs collected');
    CheckEquals(KeyOne, N[0].Key, '1st record');
    CheckEquals(KeyTwo, N[1].Key, '2nd record');
  finally
    N.Free;
  end;
end;

procedure TestTIdResourceRecords.TestCollectNameRecords;
const
  FQDNOne      = 'baz';
  IPAddressOne = '::1';
  FQDNTwo      = 'quaax';
  IPAddressTwo = '127.0.0.1';
var
  D: TIdDomainNameRecords;
begin
  Self.List.AddAAAARecord(FQDNOne, IPAddressOne);
  Self.List.AddARecord(FQDNTwo, IPAddressTwo);
  Self.List.AddCNAMERecord('foo', 'bar');
  Self.List.AddCNAMERecord('baz', 'quaax');

  D := TIdDomainNameRecords.Create;
  try
    Self.List.CollectNameRecords(D);

    CheckEquals(2, D.Count, 'Incorrect number of name records collected');
    CheckEquals(IPAddressOne, D[0].IPAddress, '1st record');
    CheckEquals(IPAddressTwo, D[1].IPAddress, '2nd record');
  finally
    D.Free;
  end;
end;

procedure TestTIdResourceRecords.TestCollectServiceRecords;
const
  DomainOne = 'one';
  DomainTwo = 'two';
var
  S: TIdSrvRecords;
begin
  Self.List.AddARecord('foo', '127.0.0.1');
  Self.List.AddARecord('bar', '127.0.0.1');
  Self.List.AddSRVRecord(DomainOne, '', 0, 0, 0, '');
  Self.List.AddSRVRecord(DomainTwo, '', 0, 0, 0, '');

  S := TIdSrvRecords.Create;
  try
    Self.List.CollectServiceRecords(S);

    CheckEquals(2, S.Count, 'Incorrect number of service records collected');
    CheckEquals(DomainOne, S[0].Domain, '1st record');
    CheckEquals(DomainTwo, S[1].Domain, '2nd record');
  finally
    S.Free;
  end;
end;

procedure TestTIdResourceRecords.TestContainsType;
begin
  Check(not Self.List.ContainsType(DnsARecord),
        'Empty list, so there should be no A records');

  Self.List.AddAAAARecord('foo', '::1');
  Check(not Self.List.ContainsType(DnsARecord),
        'Only an AAAA record');

  Self.List.AddARecord('foo', '127.0.0.1');
  Check(Self.List.ContainsType(DnsARecord),
        'AAAA, A record (looking for A)');
  Check(not Self.List.ContainsType(DnsCNAMERecord),
        'AAAA, A record (looking for CNAME)');

  Self.List.AddCNAMERecord('foo', 'bar');
  Check(Self.List.ContainsType(DnsARecord),
        'AAAA, A, CNAME record (looking for A)');
  Check(Self.List.ContainsType(DnsCNAMERecord),
        'AAAA, A, CNAME record (looking for CNAME)');
end;

initialization
  RegisterTest('SIP DNS records', Suite);
end.
