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
  Classes, IdSipMessage, Math, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipDns unit tests');
  Result.AddTest(TestTIdDomainNameRecord.Suite);
  Result.AddTest(TestTIdDomainNameRecords.Suite);
  Result.AddTest(TestTIdNaptrRecord.Suite);
  Result.AddTest(TestTIdNaptrRecords.Suite);
  Result.AddTest(TestTIdSrvRecord.Suite);
  Result.AddTest(TestTIdSrvRecords.Suite);
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
  RegisterTest('SIP DNS records', Suite);
end.
