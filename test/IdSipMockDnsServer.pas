{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockDnsServer;

interface

uses
  Classes, IdSocketHandle, IdUdpServer;

type
  TIdSipMockDnsServer = class(TObject)
  private
    AnswerIndex: Integer;
    Answers:     TStrings;
    NameServer:  TIdUdpServer;

    procedure ProvideAnswer(Sender: TObject;
                            AData: TStream;
                            ABinding: TIdSocketHandle);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddAnswer(DnsAnswer: String);
    function  AAAARecords: String;
    function  ARecords: String;
    function  ARecordProxy: String;
    function  ARecordsWithCNAME: String;
    function  ARecordsWithCNAMEOnly: String;
    procedure ClearAnswers;
    function  CNAMEChain: String;
    function  NaptrRecords: String;
    function  NaptrMissingARecord: String;
    function  NoSuchRecord: String;
    function  SrvRecords: String;
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  IdException, IdIndyUtils;

//******************************************************************************
//* TIdSipMockDnsServer                                                        *
//******************************************************************************
//* TIdSipMockDnsServer Public methods *****************************************

constructor TIdSipMockDnsServer.Create;
var
  LocalHost: TIdSocketHandle;
begin
  inherited Create;

  Self.Answers     := TStringList.Create;
  Self.AnswerIndex := 0;

  Self.NameServer := TIdUDPServer.Create(nil);
  LocalHost := Self.NameServer.Bindings.Add;
  LocalHost.IP   := '127.0.0.1';
  LocalHost.Port := 53;

  Self.NameServer.OnUDPRead     := Self.ProvideAnswer;
  Self.NameServer.ThreadedEvent := true;
  Self.Start;
end;

destructor TIdSipMockDnsServer.Destroy;
begin
  Self.Stop;
  Self.NameServer.Free;

  Self.Answers.Free;

  inherited Destroy;
end;

procedure TIdSipMockDnsServer.AddAnswer(DnsAnswer: String);
begin
  Self.Answers.Add(DnsAnswer);
end;

function TIdSipMockDnsServer.AAAARecords: String;
begin
  // Dig would translate this data as
  // ;; QUESTION SECTION:
  // ;paranoid.leo-ix.net.           IN      AAAA
  //
  // ;; ANSWER SECTION:
  // paranoid.leo-ix.net.    3600    IN      AAAA    ::1
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             3600    IN      NS      ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // ns1.leo-ix.net.         3600    IN      A       127.0.0.1

  Result :=
  { hdr id }#$85#$80#$00#$01
  + #$00#$01#$00#$01#$00#$01#$08#$70#$61#$72#$61#$6e#$6f#$69#$64#$06
  + #$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$00#$1c#$00#$01#$c0
  + #$0c#$00#$1c#$00#$01#$00#$00#$0e#$10#$00#$10#$00#$00#$00#$00#$00
  + #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$01#$c0#$15#$00#$02#$00
  + #$01#$00#$00#$0e#$10#$00#$06#$03#$6e#$73#$31#$c0#$15#$c0#$4d#$00
  + #$01#$00#$01#$00#$00#$0e#$10#$00#$04#$7f#$00#$00#$01;
end;

function TIdSipMockDnsServer.ARecords: String;
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

function TIdSipMockDnsServer.ARecordProxy: String;
begin
  // Dig would translate this data as
end;

function TIdSipMockDnsServer.ARecordsWithCNAME: String;
begin
  // Dig would translate this data as
  // ;; QUESTION SECTION:
  // ;proxy.leo-ix.net.              IN      A
  //
  // ;; ANSWER SECTION:
  // proxy.leo-ix.net.       3600    IN      CNAME   gw1.leo-ix.net.
  // gw1.leo-ix.net.         3600    IN      A       127.0.0.1
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             3600    IN      NS      ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // ns1.leo-ix.net.         3600    IN      A       127.0.0.1

  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$02#$00#$01#$00#$01#$05#$70#$72#$6f
  + #$78#$79#$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$00#$01
  + #$00#$01#$c0#$0c#$00#$05#$00#$01#$00#$00#$0e#$10#$00#$06#$03#$67
  + #$77#$31#$c0#$12#$c0#$2e#$00#$01#$00#$01#$00#$00#$0e#$10#$00#$04
  + #$7f#$00#$00#$01#$c0#$12#$00#$02#$00#$01#$00#$00#$0e#$10#$00#$06
  + #$03#$6e#$73#$31#$c0#$12#$c0#$50#$00#$01#$00#$01#$00#$00#$0e#$10
  + #$00#$04#$7f#$00#$00#$01;
end;

function TIdSipMockDnsServer.ARecordsWithCNAMEOnly: String;
begin
  // Dig would translate this data as
  // ;; QUERY SECTION:
  // ;;      proxy.leo-ix.net, type = A, class = IN
  //
  // ;; ANSWER SECTION:
  // proxy.leo-ix.net.       3600    IN      CNAME   gw1.leo-ix.net.
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             3600    IN      NS      ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // ns1.leo-ix.net.         3600    IN      A       127.0.0.1
  Result := '';
  Result :=
  { hdr id }#$85#$80#$00#$01#$00#$02#$00#$01#$00#$01#$05#$70#$72#$6f
  + #$78#$79#$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$00#$01
  + #$00#$01#$c0#$0c#$00#$05#$00#$01#$00#$00#$0e#$10#$00#$06#$03#$67
  + #$77#$31#$c0#$12#$c0#$2e#$00#$01#$00#$01#$00#$00#$0e#$10#$00#$04
  + #$7f#$00#$00#$01#$c0#$12#$00#$02#$00#$01#$00#$00#$0e#$10#$00#$06
  + #$03#$6e#$73#$31#$c0#$12#$c0#$50#$00#$01#$00#$01#$00#$00#$0e#$10
  + #$00#$04#$7f#$00#$00#$01;
end;

procedure TIdSipMockDnsServer.ClearAnswers;
begin
  Self.Answers.Clear;
end;

function TIdSipMockDnsServer.CNAMEChain: String;
begin
  // Dig would translate this data as
  // ;; QUESTION SECTION:
  // ;www.borland.com.               IN      A
  //
  // ;; ANSWER SECTION:
  // www.borland.com.        1148    IN      CNAME   www.borland.com.edgesuite.net.
  // www.borland.com.edgesuite.net. 2359 IN  CNAME   a1207.g.akamai.net.
  // a1207.g.akamai.net.     20      IN      A       196.33.166.210
  // a1207.g.akamai.net.     20      IN      A       196.33.166.208

  Result :=
  { hdr id }#$81#$80#$00#$01#$00#$04#$00#$00#$00#$00#$03#$77#$77#$77
  + #$07#$62#$6f#$72#$6c#$61#$6e#$64#$03#$63#$6f#$6d#$00#$00#$01#$00
  + #$01#$c0#$0c#$00#$05#$00#$01#$00#$00#$04#$99#$00#$1f#$03#$77#$77
  + #$77#$07#$62#$6f#$72#$6c#$61#$6e#$64#$03#$63#$6f#$6d#$09#$65#$64
  + #$67#$65#$73#$75#$69#$74#$65#$03#$6e#$65#$74#$00#$c0#$2d#$00#$05
  + #$00#$01#$00#$00#$09#$54#$00#$11#$05#$61#$31#$32#$30#$37#$01#$67
  + #$06#$61#$6b#$61#$6d#$61#$69#$c0#$47#$c0#$58#$00#$01#$00#$01#$00
  + #$00#$00#$14#$00#$04#$c4#$21#$a6#$d0#$c0#$58#$00#$01#$00#$01#$00
  + #$00#$00#$14#$00#$04#$c4#$21#$a6#$d2;
end;

function TIdSipMockDnsServer.NaptrRecords: String;
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

function TIdSipMockDnsServer.NaptrMissingARecord: String;
begin
  // Dig would translate this data as
  // ;; ANSWER SECTION:
  // leo-ix.net.             3600    IN      NAPTR   0 0 "s" "SIP+D2U" "" _sip._udp.leo-ix.net.
  // leo-ix.net.             3600    IN      NAPTR   0 0 "s" "SIPS+D2T" "" _sips._tcp.leo-ix.net.
  // leo-ix.net.             3600    IN      NAPTR   0 0 "s" "SIP+D2T" "" _sip._tcp.leo-ix.net.
  //
  // ;; AUTHORITY SECTION:
  // leo-ix.net.             3600    IN      NS      ns1.leo-ix.net.
  //
  // ;; ADDITIONAL SECTION:
  // paranoid-bak.leo-ix.net. 3600   IN      A       127.0.1.1
  // paranoid.leo-ix.net.    3600    IN      A       127.0.0.1
  // paranoid.leo-ix.net.    3600    IN      A       127.0.0.2
  // paranoid.leo-ix.net.    3600    IN      AAAA    ::1
  // ns1.leo-ix.net.         3600    IN      A       127.0.0.1
  // _sip._tcp.leo-ix.net.   3600    IN      SRV     2 0 5060 sip-proxy.leo-ix.net.
  // _sip._udp.leo-ix.net.   3600    IN      SRV     3 0 5060 sip-proxy.leo-ix.net.
  // _sips._tcp.leo-ix.net.  3600    IN      SRV     1 1 5061 paranoid-bak.leo-ix.net.
  // _sips._tcp.leo-ix.net.  3600    IN      SRV     1 2 5061 paranoid.leo-ix.net.

  Result :=
  { hdr id }#$85#$80
  + #$00#$01#$00#$03#$00#$01#$00#$09#$06#$6c#$65#$6f#$2d#$69#$78#$03
  + #$6e#$65#$74#$00#$00#$23#$00#$01#$c0#$0c#$00#$23#$00#$01#$00#$00
  + #$0e#$10#$00#$25#$00#$00#$00#$00#$01#$73#$07#$53#$49#$50#$2b#$44
  + #$32#$55#$00#$04#$5f#$73#$69#$70#$04#$5f#$75#$64#$70#$06#$6c#$65
  + #$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$c0#$41#$00#$23#$00#$01#$00
  + #$00#$0e#$10#$00#$27#$00#$00#$00#$00#$01#$73#$08#$53#$49#$50#$53
  + #$2b#$44#$32#$54#$00#$05#$5f#$73#$69#$70#$73#$04#$5f#$74#$63#$70
  + #$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$c0#$74#$00#$23
  + #$00#$01#$00#$00#$0e#$10#$00#$25#$00#$00#$00#$00#$01#$73#$07#$53
  + #$49#$50#$2b#$44#$32#$54#$00#$04#$5f#$73#$69#$70#$04#$5f#$74#$63
  + #$70#$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$c0#$a5#$00
  + #$02#$00#$01#$00#$00#$0e#$10#$00#$06#$03#$6e#$73#$31#$c0#$a5#$0c
  + #$70#$61#$72#$61#$6e#$6f#$69#$64#$2d#$62#$61#$6b#$c0#$a5#$00#$01
  + #$00#$01#$00#$00#$0e#$10#$00#$04#$7f#$00#$01#$01#$08#$70#$61#$72
  + #$61#$6e#$6f#$69#$64#$c0#$a5#$00#$01#$00#$01#$00#$00#$0e#$10#$00
  + #$04#$7f#$00#$00#$01#$c0#$e0#$00#$01#$00#$01#$00#$00#$0e#$10#$00
  + #$04#$7f#$00#$00#$02#$c0#$e0#$00#$1c#$00#$01#$00#$00#$0e#$10#$00
  + #$10#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00
  + #$01#$c0#$bd#$00#$01#$00#$01#$00#$00#$0e#$10#$00#$04#$7f#$00#$00
  + #$01#$c0#$9b#$00#$21#$00#$01#$00#$00#$0e#$10#$00#$1c#$00#$02#$00
  + #$00#$13#$c4#$09#$73#$69#$70#$2d#$70#$72#$6f#$78#$79#$06#$6c#$65
  + #$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$c0#$37#$00#$21#$00#$01#$00
  + #$00#$0e#$10#$00#$1c#$00#$03#$00#$00#$13#$c4#$09#$73#$69#$70#$2d
  + #$70#$72#$6f#$78#$79#$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74
  + #$00#$c0#$69#$00#$21#$00#$01#$00#$00#$0e#$10#$00#$1f#$00#$01#$00
  + #$01#$13#$c5#$0c#$70#$61#$72#$61#$6e#$6f#$69#$64#$2d#$62#$61#$6b
  + #$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e#$65#$74#$00#$c0#$69#$00#$21
  + #$00#$01#$00#$00#$0e#$10#$00#$1b#$00#$01#$00#$02#$13#$c5#$08#$70
  + #$61#$72#$61#$6e#$6f#$69#$64#$06#$6c#$65#$6f#$2d#$69#$78#$03#$6e
  + #$65#$74#$00;
end;

function TIdSipMockDnsServer.NoSuchRecord: String;
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

function TIdSipMockDnsServer.SrvRecords: String;
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

procedure TIdSipMockDnsServer.Start;
begin
  try
    Self.NameServer.Active := true;
  except
    on E: EIdException do
      RaiseSocketError(E, Self.NameServer.Bindings);
  end;
end;

procedure TIdSipMockDnsServer.Stop;
begin
  Self.NameServer.Active := false;
end;

//* TIdSipMockDnsServer Private methods ****************************************

procedure TIdSipMockDnsServer.ProvideAnswer(Sender: TObject;
                                            AData: TStream;
                                            ABinding: TIdSocketHandle);
var
  Answer:  String;
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

  Answer := ReplyID + Self.Answers[Self.AnswerIndex];

  Self.NameServer.Send(ABinding.PeerIP,
                       ABinding.PeerPort,
                       Answer);
  Inc(Self.AnswerIndex);
end;

end.
