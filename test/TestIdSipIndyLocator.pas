unit TestIdSipIndyLocator;

interface

uses
  Classes, IdSipIndyLocator, IdSipLocator, IdSocketHandle, IdUdpServer,
  TestFramework;

type
  // The NameServer provides hard-coded answers to the questions. The important
  // part, the bit we're actually testing, is that the locator Loc can correctly
  // parse and present the answer.
  TestTIdSipIndyLocator = class(TTestCase)
  private
    Answer:     String;
    Loc:        TIdSipIndyLocator;
    NameRecs:   TIdDomainNameRecords;
    NameServer: TIdUdpServer;
    NaptrRecs:  TIdNaptrRecords;
    SrvRecs:    TIdSrvRecords;

    function  ARecords: String;
    function  NaptrRecords: String;
    function  NoSuchRecord: String;

    procedure ProvideAnswer(Sender: TObject;
                            AData: TStream;
                            ABinding: TIdSocketHandle);
    function SrvRecords: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveNameRecords;
    procedure TestResolveNameRecordsNetworkFailure;
    procedure TestResolveNameRecordsOnNonexistentDomain;
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
var
  LocalHost: TIdSocketHandle;
begin
  inherited SetUp;

  Self.Loc := TIdSipIndyLocator.Create;
  Self.Loc.NameServer := '127.0.0.1';

  Self.NameServer := TIdUDPServer.Create(nil);

  LocalHost := Self.NameServer.Bindings.Add;
  LocalHost.IP   := '127.0.0.1';
  LocalHost.Port := 53;

  Self.NameRecs  := TIdDomainNameRecords.Create;
  Self.NaptrRecs := TIdNaptrRecords.Create;
  Self.SrvRecs   := TIdSrvRecords.Create;

  Self.NameServer.OnUDPRead     := Self.ProvideAnswer;
  Self.NameServer.ThreadedEvent := true;
  Self.NameServer.Active        := true;
end;

procedure TestTIdSipIndyLocator.TearDown;
begin
  Self.SrvRecs.Free;
  Self.NaptrRecs.Free;
  Self.NameRecs.Free;

  Self.NameServer.Active := false;
  Self.NameServer.Free;
  Self.Loc.Free;

  inherited TearDown;
end;

//* TestTIdSipIndyLocator Private methods **************************************

function TestTIdSipIndyLocator.ARecords: String;
begin

  // Dig would translate this data as
  // ;; QUERY SECTION:
  // ;;      paranoid.leo-ix.net, type = A, class = IN
  //
  // ;; ANSWER SECTION:
  // paranoid.leo-ix.net.    1H IN A         127.0.0.2
  // paranoid.leo-ix.net.    1H IN A         127.0.0.1
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             1H IN NS        ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // ns1.leo-ix.net.         1H IN A         127.0.0.1

  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$02#$00#$01#$00#$01#$08#$70#$61#$72
  + #$61#$6E#$6F#$69#$64#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74
  + #$00#$00#$01#$00#$01#$C0#$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00
  + #$04#$7F#$00#$00#$01#$C0#$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00
  + #$04#$7F#$00#$00#$02#$C0#$15#$00#$02#$00#$01#$00#$00#$0E#$10#$00
  + #$06#$03#$6E#$73#$31#$C0#$15#$C0#$51#$00#$01#$00#$01#$00#$00#$0E
  + #$10#$00#$04#$7F#$00#$00#$01;
end;

function TestTIdSipIndyLocator.NaptrRecords: String;
begin

  // Dig would translate this data as
  // ;; QUERY SECTION:
  // ;;      leo-ix.net, type = NAPTR, class = IN
  //
  // ;; ANSWER SECTION:
  // leo-ix.net.             1H IN NAPTR     0 0 "s" "SIPS+D2T" "" _sips._tcp.leo-ix.net.
  // leo-ix.net.             1H IN NAPTR     0 0 "s" "SIP+D2T" "" _sip._tcp.leo-ix.net.
  // leo-ix.net.             1H IN NAPTR     0 0 "s" "SIP+D2U" "" _sip._udp.leo-ix.net.
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             1H IN NS        ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // _sips._tcp.leo-ix.net.  1H IN SRV       1 2 5061 paranoid.leo-ix.net.
  // _sips._tcp.leo-ix.net.  1H IN SRV       1 1 5061 paranoid-bak.leo-ix.net.
  // _sip._tcp.leo-ix.net.   1H IN SRV       2 0 5060 sip-proxy.leo-ix.net.
  // _sip._udp.leo-ix.net.   1H IN SRV       3 0 5060 sip-proxy.leo-ix.net.
  // ns1.leo-ix.net.         1H IN A         127.0.0.1
  // paranoid.leo-ix.net.    1H IN A         127.0.0.1
  // paranoid.leo-ix.net.    1H IN A         127.0.0.2
  // paranoid-bak.leo-ix.net.  1H IN A  127.0.1.1
  // sip-proxy.leo-ix.net.   1H IN A         127.0.0.2

  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$03#$00#$01#$00#$09#$06#$6C#$65#$6F
  + #$2D#$69#$78#$03#$6E#$65#$74#$00#$00#$23#$00#$01#$C0#$0C#$00#$23
  + #$00#$01#$00#$00#$0E#$10#$00#$25#$00#$00#$00#$00#$01#$73#$07#$53
  + #$49#$50#$2B#$44#$32#$55#$00#$04#$5F#$73#$69#$70#$04#$5F#$75#$64
  + #$70#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74#$00#$C0#$0C#$00
  + #$23#$00#$01#$00#$00#$0E#$10#$00#$27#$00#$00#$00#$00#$01#$73#$08
  + #$53#$49#$50#$53#$2B#$44#$32#$54#$00#$05#$5F#$73#$69#$70#$73#$04
  + #$5F#$74#$63#$70#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74#$00
  + #$C0#$0C#$00#$23#$00#$01#$00#$00#$0E#$10#$00#$25#$00#$00#$00#$00
  + #$01#$73#$07#$53#$49#$50#$2B#$44#$32#$54#$00#$04#$5F#$73#$69#$70
  + #$04#$5F#$74#$63#$70#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74
  + #$00#$C0#$0C#$00#$02#$00#$01#$00#$00#$0E#$10#$00#$06#$03#$6E#$73
  + #$31#$C0#$0C#$04#$5F#$73#$69#$70#$04#$5F#$75#$64#$70#$C0#$0C#$00
  + #$21#$00#$01#$00#$00#$0E#$10#$00#$1C#$00#$03#$00#$00#$13#$C4#$09
  + #$73#$69#$70#$2D#$70#$72#$6F#$78#$79#$06#$6C#$65#$6F#$2D#$69#$78
  + #$03#$6E#$65#$74#$00#$05#$5F#$73#$69#$70#$73#$04#$5F#$74#$63#$70
  + #$C0#$0C#$00#$21#$00#$01#$00#$00#$0E#$10#$00#$1B#$00#$01#$00#$02
  + #$13#$C5#$08#$70#$61#$72#$61#$6E#$6F#$69#$64#$06#$6C#$65#$6F#$2D
  + #$69#$78#$03#$6E#$65#$74#$00#$C0#$F5#$00#$21#$00#$01#$00#$00#$0E
  + #$10#$00#$1F#$00#$01#$00#$01#$13#$C5#$0C#$70#$61#$72#$61#$6E#$6F
  + #$69#$64#$2D#$62#$61#$6B#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65
  + #$74#$00#$04#$5F#$73#$69#$70#$C0#$FB#$00#$21#$00#$01#$00#$00#$0E
  + #$10#$00#$1C#$00#$02#$00#$00#$13#$C4#$09#$73#$69#$70#$2D#$70#$72
  + #$6F#$78#$79#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74#$00#$C0
  + #$BD#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00#$00#$01#$09
  + #$73#$69#$70#$2D#$70#$72#$6F#$78#$79#$C0#$0C#$00#$01#$00#$01#$00
  + #$00#$0E#$10#$00#$04#$7F#$00#$00#$02#$08#$70#$61#$72#$61#$6E#$6F
  + #$69#$64#$C0#$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00
  + #$00#$01#$C1#$A9#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00
  + #$00#$02#$0C#$70#$61#$72#$61#$6E#$6F#$69#$64#$2D#$62#$61#$6B#$C0
  + #$0C#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00#$01#$01;
end;

function TestTIdSipIndyLocator.NoSuchRecord: String;
begin

  // Dig would translate this data as
  // ;; res options: init recurs defnam dnsrch
  // ;; got answer:
  // ;; ->>HEADER<<- opcode: QUERY, status: NXDOMAIN, id: <id>
  // ;; flags: qr rd ra; QUERY: 1, ANSWER: 0, AUTHORITY: 1, ADDITIONAL: 0
  // ;; QUERY SECTION:
  // ;;      foo.bar, type = A, class = IN
  //
  // ;; AUTHORITY SECTION:
  // .                       1h37m30s IN SOA  A.ROOT-SERVERS.NET. NSTLD.VERISIGN-GRS.COM. (
  //                                         2005030701      ; serial
  //                                         30M             ; refresh
  //                                         15M             ; retry
  //                                         1W              ; expiry
  //                                         1D )            ; minimum

  Result :=
  { hdr id }#$81#$83#$00#$01#$00#$00#$00#$01#$00#$00#$03#$66#$6F#$6F
  + #$03#$62#$61#$72#$00#$00#$01#$00#$01#$00#$00#$06#$00#$01#$00#$00
  + #$2A#$1A#$00#$40#$01#$41#$0C#$52#$4F#$4F#$54#$2D#$53#$45#$52#$56
  + #$45#$52#$53#$03#$4E#$45#$54#$00#$05#$4E#$53#$54#$4C#$44#$0C#$56
  + #$45#$52#$49#$53#$49#$47#$4E#$2D#$47#$52#$53#$03#$43#$4F#$4D#$00
  + #$77#$82#$57#$2D#$00#$00#$07#$08#$00#$00#$03#$84#$00#$09#$3A#$80
  + #$00#$01#$51#$80;
end;

procedure TestTIdSipIndyLocator.ProvideAnswer(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
var
  ReplyID: String;
  S:       TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(AData, 0);

    ReplyID := Copy(S.DataString, 1, 2);
  finally
    S.Free;
  end;

  Self.Answer := ReplyID + Self.Answer;

  Self.NameServer.Send(ABinding.PeerIP,
                       ABinding.PeerPort,
                       Self.Answer);
end;

function TestTIdSipIndyLocator.SrvRecords: String;
begin
  // Dig would translate this data as
  // ;; QUERY SECTION:
  // ;;      _sips._tcp.leo-ix.net, type = SRV, class = IN
  //
  // ;; ANSWER SECTION:
  // _sips._tcp.leo-ix.net.  1H IN SRV       1 2 5061 paranoid.leo-ix.net.
  // _sips._tcp.leo-ix.net.  1H IN SRV       1 1 5061 paranoid-bak.leo-ix.net.
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             1H IN NS        ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // paranoid.leo-ix.net.    1H IN A         127.0.0.1
  // paranoid.leo-ix.net.    1H IN A         127.0.0.2
  // paranoid-bak.leo-ix.net.  1H IN A  127.0.1.1
  // ns1.leo-ix.net.         1H IN A         127.0.0.1

  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$02#$00#$01#$00#$04#$05#$5F#$73#$69
  + #$70#$73#$04#$5F#$74#$63#$70#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E
  + #$65#$74#$00#$00#$21#$00#$01#$C0#$0C#$00#$21#$00#$01#$00#$00#$0E
  + #$10#$00#$1B#$00#$01#$00#$02#$13#$C5#$08#$70#$61#$72#$61#$6E#$6F
  + #$69#$64#$06#$6C#$65#$6F#$2D#$69#$78#$03#$6E#$65#$74#$00#$C0#$0C
  + #$00#$21#$00#$01#$00#$00#$0E#$10#$00#$1F#$00#$01#$00#$01#$13#$C5
  + #$0C#$70#$61#$72#$61#$6E#$6F#$69#$64#$2D#$62#$61#$6B#$06#$6C#$65
  + #$6F#$2D#$69#$78#$03#$6E#$65#$74#$00#$C0#$17#$00#$02#$00#$01#$00
  + #$00#$0E#$10#$00#$06#$03#$6E#$73#$31#$C0#$17#$08#$70#$61#$72#$61
  + #$6E#$6F#$69#$64#$C0#$17#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04
  + #$7F#$00#$00#$01#$C0#$8B#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04
  + #$7F#$00#$00#$02#$0C#$70#$61#$72#$61#$6E#$6F#$69#$64#$2D#$62#$61
  + #$6B#$C0#$17#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00#$01
  + #$01#$C0#$85#$00#$01#$00#$01#$00#$00#$0E#$10#$00#$04#$7F#$00#$00
  + #$01
end;

//* TestTIdSipIndyLocator Published methods ************************************

procedure TestTIdSipIndyLocator.TestResolveNameRecords;
begin
  Self.Answer := Self.ARecords;

  Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Self.NameRecs);

  // See the comment in Self.ARecords.
  CheckEquals(2, Self.NameRecs.Count, 'Record count');

  CheckEquals(DnsARecord,            Self.NameRecs[0].RecordType, '1st record record type');
  CheckEquals('paranoid.leo-ix.net', Self.NameRecs[0].Domain,     '1st record domain');
  CheckEquals('127.0.0.1',           Self.NameRecs[0].IPAddress,  '1st record IP address');

  CheckEquals(DnsARecord,            Self.NameRecs[1].RecordType, '2nd record record type');
  CheckEquals('paranoid.leo-ix.net', Self.NameRecs[1].Domain,     '2nd record domain');
  CheckEquals('127.0.0.2',           Self.NameRecs[1].IPAddress,  '2nd record IP address');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsNetworkFailure;
begin
  // This shows what happens when something on the network goes wrong (like,
  // say, the name server disappearing).
  Self.NameServer.Active := false;

  Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Self.NameRecs);

  CheckEquals(0, Self.NameRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveNameRecordsOnNonexistentDomain;
begin
  Self.Answer := Self.NoSuchRecord;

  Self.Loc.ResolveNameRecords('foo.bar', Self.NameRecs);

  CheckEquals(0, Self.NameRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveNAPTR;
var
  Uri: TIdSipUri;
begin
  Self.Answer := Self.NaptrRecords;

  Uri := TIdSipUri.Create('sip:leo-ix.net');
  try
    Self.Loc.ResolveNAPTR(Uri, Self.NaptrRecs);
    // See the comment in Self.NaptrRecords.
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
  Self.NameServer.Active := false;

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
  Self.Answer := Self.NoSuchRecord;

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
  Self.Answer := Self.SrvRecords;

  Self.Loc.ResolveSRV('_sip._tcp.leo-ix.net', Self.SrvRecs);

  // See the comment in Self.SrvRecords.
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
  Self.NameServer.Active := false;

  Self.Loc.ResolveSRV('foo.bar', Self.SrvRecs);

  CheckEquals(0, Self.SrvRecs.Count, 'Record count');
end;

procedure TestTIdSipIndyLocator.TestResolveSRVOnNonexistentDomain;
begin
  Self.Answer := Self.NoSuchRecord;

  Self.Loc.ResolveSRV('foo.bar', Self.SrvRecs);

  CheckEquals(0, Self.SrvRecs.Count, 'Record count');
end;

initialization
  RegisterTest('Indy DNS lookup routines', Suite);
end.
