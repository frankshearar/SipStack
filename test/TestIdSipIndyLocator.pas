unit TestIdSipIndyLocator;

interface

uses
  Classes, IdSipIndyLocator, IdSocketHandle, IdUdpServer, TestFramework;

type
  // The NameServer provides hard-coded answers to the questions. The important
  // part, the bit we're actually testing, is that the locator Loc can correctly
  // parse and present the answer.
  TestTIdSipIndyLocator = class(TTestCase)
  private
    Answer:     String;
    Loc:        TIdSipIndyLocator;
    NameServer: TIdUdpServer;

    function  ARecords: String;
    function  NaptrRecords: String;

    procedure ProvideAnswer(Sender: TObject;
                            AData: TStream;
                            ABinding: TIdSocketHandle);
    function SrvRecords: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveNameRecords;
    procedure TestResolveNAPTR;
    procedure TestResolveSRV;
  end;

implementation

uses
  IdSipLocator, IdSipMessage;

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

  Self.NameServer.OnUDPRead     := Self.ProvideAnswer;
  Self.NameServer.ThreadedEvent := true;
  Self.NameServer.Active        := true;
end;

procedure TestTIdSipIndyLocator.TearDown;
begin
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
var
  Records: TIdDomainNameRecords;
begin
  Self.Answer := Self.ARecords;

  Records := TIdDomainNameRecords.Create;
  try
    Self.Loc.ResolveNameRecords('paranoid.leo-ix.net', Records);

    // See the comment in Self.ARecords.
    CheckEquals(2, Records.Count, 'Record count');

    CheckEquals(DnsARecord,            Records[0].RecordType, '1st record record type');
    CheckEquals('paranoid.leo-ix.net', Records[0].Domain,     '1st record domain');
    CheckEquals('127.0.0.1',           Records[0].IPAddress,  '1st record IP address');

    CheckEquals(DnsARecord,            Records[1].RecordType, '2nd record record type');
    CheckEquals('paranoid.leo-ix.net', Records[1].Domain,     '2nd record domain');
    CheckEquals('127.0.0.2',           Records[1].IPAddress,  '2nd record IP address');
  finally
    Records.Free;
  end;
end;

procedure TestTIdSipIndyLocator.TestResolveNAPTR;
var
  Records: TIdNaptrRecords;
  Uri:     TIdSipUri;
begin
  Self.Answer := Self.NaptrRecords;

  Uri := TIdSipUri.Create('sip:leo-ix.net');
  try
    Records := TIdNaptrRecords.Create;
    try
      Self.Loc.ResolveNAPTR(Uri, Records);
      // See the comment in Self.NaptrRecords.
      CheckEquals(3, Records.Count, 'Record count');

      CheckEquals('s',                     Records[0].Flags,      '1st record Flag');
      CheckEquals('leo-ix.net',            Records[0].Key,        '1st record Key');
      CheckEquals(0,                       Records[0].Order,      '1st record Order');
      CheckEquals(0,                       Records[0].Preference, '1st record Preference');
      CheckEquals('',                      Records[0].Regex,      '1st record Regex');
      CheckEquals('SIPS+D2T',              Records[0].Service,    '1st record Service');
      CheckEquals('_sips._tcp.leo-ix.net', Records[0].Value,      '1st record Value');

      CheckEquals('s',                     Records[1].Flags,      '2nd record Flag');
      CheckEquals('leo-ix.net',            Records[1].Key,        '2nd record Key');
      CheckEquals(0,                       Records[1].Order,      '2nd record Order');
      CheckEquals(0,                       Records[1].Preference, '2nd record Preference');
      CheckEquals('',                      Records[1].Regex,      '2nd record Regex');
      CheckEquals('SIP+D2T',               Records[1].Service,    '2nd record Service');
      CheckEquals('_sip._tcp.leo-ix.net',  Records[1].Value,      '2nd record Value');

      CheckEquals('s',                     Records[2].Flags,      '3rd record Flag');
      CheckEquals('leo-ix.net',            Records[2].Key,        '3rd record Key');
      CheckEquals(0,                       Records[2].Order,      '3rd record Order');
      CheckEquals(0,                       Records[2].Preference, '3rd record Preference');
      CheckEquals('',                      Records[2].Regex,      '3rd record Regex');
      CheckEquals('SIP+D2U',               Records[2].Service,    '3rd record Service');
      CheckEquals('_sip._udp.leo-ix.net',  Records[2].Value,      '3rd record Value');
    finally
      Records.Free;
    end;
  finally
    Uri.Free;
  end;
end;

procedure TestTIdSipIndyLocator.TestResolveSRV;
var
  Records: TIdSrvRecords;
begin
  Self.Answer := Self.SrvRecords;

  Records := TIdSrvRecords.Create;
  try
    Self.Loc.ResolveSRV('_sip._tcp.leo-ix.net', Records);

    // See the comment in Self.SrvRecords.
    CheckEquals(2, Records.Count, 'Record count');

    CheckEquals('leo-ix.net',          Records[0].Domain,   '1st record Domain');
    CheckEquals(5061,                  Records[0].Port,     '1st record Port');
    CheckEquals(1,                     Records[0].Priority, '1st record Priority');
    CheckEquals('_sips._tcp',          Records[0].Service,  '1st record Service');
    CheckEquals('paranoid.leo-ix.net', Records[0].Target,   '1st record Target');
    CheckEquals(2,                     Records[0].Weight,   '1st record Weight');

    CheckEquals(2, Records[0].NameRecords.Count, '1st record name record count');
    CheckEquals('paranoid.leo-ix.net',
                Records[0].NameRecords[0].Domain,
                '1st record 1st name record');
    CheckEquals('127.0.0.1',
                Records[0].NameRecords[0].IPAddress,
                '1st record 1st name record IP address');
    CheckEquals(DnsARecord,
                Records[0].NameRecords[0].RecordType,
                '1st record 1st name record type');

    CheckEquals('paranoid.leo-ix.net',
                Records[0].NameRecords[1].Domain,
                '1st record 2nd name record');
    CheckEquals('127.0.0.2',
                Records[0].NameRecords[1].IPAddress,
                '1st record 2nd name record IP address');
    CheckEquals(DnsARecord,
                Records[0].NameRecords[1].RecordType,
                '1st record 2nd name record type');

    CheckEquals('leo-ix.net',              Records[1].Domain,   '2nd record Domain');
    CheckEquals(5061,                      Records[1].Port,     '2nd record Port');
    CheckEquals(1,                         Records[1].Priority, '2nd record Priority');
    CheckEquals('_sips._tcp',              Records[1].Service,  '2nd record Service');
    CheckEquals('paranoid-bak.leo-ix.net', Records[1].Target,   '2nd record Target');
    CheckEquals(1,                         Records[1].Weight,   '2nd record Weight');

    CheckEquals(1, Records[1].NameRecords.Count, '2nd record name record count');
    CheckEquals('paranoid-bak.leo-ix.net',
                Records[1].NameRecords[0].Domain,
                '2nd record 1st name record');
    CheckEquals('127.0.1.1',
                Records[1].NameRecords[0].IPAddress,
                '2nd record 1st name record IP address');
    CheckEquals(DnsARecord,
                Records[1].NameRecords[0].RecordType,
                '2nd record 1st name record type');
  finally
    Records.Free;
  end;
end;

initialization
  RegisterTest('Indy DNS lookup routines', Suite);
end.
