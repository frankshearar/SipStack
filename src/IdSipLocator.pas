{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipLocator;

interface

uses
  Classes, Contnrs, IdSipMessage;

// The classes below all encapsulate DNS lookups, or the SIP processing of these
// lookups according to RFC 3263.
//
// As a refresher, here's what a sample zonefile looks like:
//
// $TTL 3600
// @       IN      SOA     ns1.foo.bar.  dnsadmin.foo.bar. (
//                        1998082085
//                        1800            ; refresh, seconds
//                        900             ; retry, seconds
//                        604800          ; expire, seconds
//                        86400 )         ; minimum, seconds
//                A       1.1.1.1
//                NS      ns1.foo.bar.
//                NS      ns.quaax.quaax.
//                MX      100 mx1.foo.bar.
//                MX      200 mx2.foo.bar.
//
// ;;               Priority Weight Port   Target
//  _sips._tls  SRV 0        0      5060   baz.foo.bar.
//  _sip._tcp   SRV 0        0      5060   baz.foo.bar.
//  _sip._udp   SRV 0        0      5060   baz.foo.bar.
//
// ;;        order pref flags service      regexp  replacement
//  IN NAPTR 50    50   "s"   "SIPS+D2T"   ""      _sips._tcp.baz.foo.bar.
//  IN NAPTR 90    50   "s"   "SIP+D2T"    ""      _sip._tcp.baz.foo.bar.
//  IN NAPTR 100   50   "s"   "SIP+D2U"    ""      _sip._udp.baz.foo.bar.
//
// localhost       A       127.0.0.1
// warez           A       127.0.0.1
// baz             A       1.1.1.2
// sip             A       1.1.1.2
// baz             AAAA    2002:0101:0102:1::1
// sip             AAAA    2002:0101:0102:1::1

type
  TIdSipLocation = class(TObject)
  private
    fTransport: String;
    fAddress:   String;
    fPort:      Cardinal;
  public
    constructor Create(const Transport: String;
                       const Address:   String;
                             Port: Cardinal); overload;
    constructor Create(Via: TIdSipViaHeader); overload;

    function Copy: TIdSipLocation;

    property Transport: String   read fTransport;
    property Address:   String   read fAddress;
    property Port:      Cardinal read fPort;
  end;

  TIdSipLocations = class(TObject)
  private
    List: TObjectList;

    function GetLocation(Index: Integer): TIdSipLocation;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddLocation(const Transport: String;
                          const Address: String;
                          Port: Cardinal);
    function  Count: Integer;
    function  First: TIdSipLocation;
    function  IsEmpty: Boolean;

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

  TIdNaptrRecords = class;

  // Given a SIP or SIPS URI, I return (using FindServersFor) a set of tuples of
  // the form (transport, IP address, port) that you can use to send a SIP
  // message.
  TIdSipAbstractLocator = class(TObject)
  protected
    procedure AddUriLocation(AddressOfRecord: TIdSipUri;
                             List: TIdSipLocations);
    function  CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
  public
    constructor Create; virtual;

    function FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; overload; 
    function FindServersFor(const AddressOfRecord: String): TIdSipLocations; overload;
    function FindServersFor(Response: TIdSipResponse): TIdSipLocations; overload;
    function ResolveNameRecords(const DomainName: String): TStrings; virtual; abstract;
    function ResolveNAPTR(const DomainName: String): TIdNaptrRecords; virtual; abstract;
  end;

  // I take an address-of-record SIP/SIPS URI and return a URL at which
  // a server can take a call for the given address-of-record.
  TIdSipLocator = class(TIdSipAbstractLocator)
  public
    function ResolveNameRecords(const DomainName: String): TStrings; override;
    function ResolveNAPTR(const DomainName: String): TIdNaptrRecords; override;
  end;

  TIdDomainNameRecord = class(TObject)
  private
    fDomain:     String;
    fIPAddress:  String;
    fRecordType: String;
  public
    constructor Create(const RecordType, Domain, IPAddress: String);

    function Copy: TIdDomainNameRecord;

    property Domain:     String read fDomain;
    property IPAddress:  String read fIPAddress;
    property RecordType: String read fRecordType;
  end;

  // cf RFCs 2915, 3401-3
  // I represent a single NAPTR record.
  TIdNaptrRecord = class(TObject)
  private
    fFlags:      String;
    fKey:        String; // The DDDS key
    fOrder:      Word;
    fPreference: Word;
    fRegex:      String;
    fService:    String;
    fValue:      String; // The DDDS value (a domain name we can feed into SRV queries
  public
    constructor Create(const Key: String;
                       Order: Word;
                       Preference: Word;
                       const Flags: String;
                       const Service: String;
                       const Regex: String;
                       const Value: String);
    function Copy: TIdNaptrRecord;

    property Flags:      String read fFlags;
    property Key:        String read fKey;
    property Order:      Word   read fOrder;
    property Preference: Word   read fPreference;
    property Regex:      String read fRegex;
    property Service:    String read fService;
    property Value:      String read fValue;
  end;

  // I represent a list of NAPTR records, usually a result of a NAPTR query.
  TIdNaptrRecords = class(TObject)
  private
    List: TObjectList;

    function GetItems(Index: Integer): TIdNaptrRecord;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Copy: TIdNaptrRecord);
    procedure Clear;
    function  Count: Integer;

    property Items[Index: Integer]: TIdNaptrRecord read GetItems; default;
  end;

const
  DnsARecord     = 'A';
  DnsA6Record    = 'A6';
  DnsAAAARecord  = 'AAAA';
  SipSctpService = 'SIP+D2S';
  SipsTlsService = 'SIPS+D2T';
  SipTcpService  = 'SIP+D2T';
  SipUdpService  = 'SIP+D2U';
  SrvSctpPrefix  = '_sip._sctp';
  SrvTcpPrefix   = '_sip._tcp';
  SrvUdpPrefix   = '_sip._udp';
  SrvTlsPrefix   = '_sips._tcp';

implementation

uses
  IdSimpleParser, SysUtils;

//******************************************************************************
//* TIdSipLocation                                                             *
//******************************************************************************
//* TIdSipLocation Public methods **********************************************

constructor TIdSipLocation.Create(const Transport: String;
                                  const Address:   String;
                                  Port: Cardinal);
begin
  inherited Create;

  Self.fTransport := Transport;
  Self.fAddress   := Address;
  Self.fPort      := Port;
end;

constructor TIdSipLocation.Create(Via: TIdSipViaHeader);
begin
  inherited Create;

  Self.fTransport := Via.Transport;
  Self.fAddress   := Via.SentBy;
  Self.fPort      := Via.Port;
end;

function TIdSipLocation.Copy: TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Self.Transport, Self.Address, Self.Port);
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

constructor TIdSipLocations.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipLocations.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSipLocations.AddLocation(const Transport: String;
                                      const Address: String;
                                      Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Add(NewLocation);
end;

function TIdSipLocations.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  Result := Self.Items[0];
end;

function TIdSipLocations.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

//* TIdSipLocations Private methods ********************************************

function TIdSipLocations.GetLocation(Index: Integer): TIdSipLocation;
begin
  Result := Self.List[Index] as TidSipLocation;
end;

//******************************************************************************
//* TIdSipAbstractLocator                                                      *
//******************************************************************************
//* TIdSipAbstractLocator Public methods ***************************************

constructor TIdSipAbstractLocator.Create;
begin
  inherited Create;
end;

function TIdSipAbstractLocator.FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations;
var
  Target: String;
  UriTransport: String;
begin
  Result := TIdSipLocations.Create;

  if AddressOfRecord.HasMaddr then
    Target := AddressOfRecord.Maddr
  else
    Target := AddressOfRecord.Host;

  if AddressOfRecord.TransportIsSpecified then begin
    Result.AddLocation(ParamToTransport(AddressOfRecord.Transport),
                       Target,
                       AddressOfRecord.Port);
    Exit;
  end
  else begin
    if TIdIPAddressParser.IsNumericAddress(Target) or AddressOfRecord.PortIsSpecified then begin
      if AddressOfRecord.IsSecure then
        UriTransport := TlsTransport
      else
        UriTransport := UdpTransport;
    end;

    Result.AddLocation(UriTransport, Target, AddressOfRecord.Port);
    Exit;
  end;

  raise Exception.Create('No NAPTR stuff');
  raise Exception.Create('No SRV stuff');
  raise Exception.Create('No A/AAAA stuff');
end;

function TIdSipAbstractLocator.FindServersFor(const AddressOfRecord: String): TIdSipLocations;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create(AddressOfRecord);
  try
    Result := Self.FindServersFor(Uri);
  finally
    Uri.Free;
  end;
end;

function TIdSipAbstractLocator.FindServersFor(Response: TIdSipResponse): TIdSipLocations;
begin
  // cf RFC 3262, section 6:
  // Sending (unicast) responses:
  // 1.  Try send it down the existing connection (if TCP) or to the source
  //     IP/port (if UDP).
  // 2.  Look at the sent-by of the top Via.
  // 2.1 Numeric IP? Attempt the transport/IP/port in the top Via
  // 2.2 Name and port? Query for A/AAAA records; iterate over the
  //     list. Use the transport and port from the top Via.
  // 2.3 Name and no port? Query SRV for that name using "_sips" if TLS or
  //     "_sip" otherwise. Iterate over the list using the transport in the
  //     sent-by and the IP/ports from the SRV query.

  Result := TIdSipLocations.Create;

  if Response.LastHop.HasReceived then
    Result.AddLocation(Response.LastHop.Transport,
                       Response.LastHop.Received,
                       Response.LastHop.Port);

  if TIdIPAddressParser.IsIPv4Address(Response.LastHop.SentBy)
  or TIdIPAddressParser.IsIPv6Address(Response.LastHop.SentBy) then
    Result.AddLocation(Response.LastHop.Transport,
                       Response.LastHop.SentBy,
                       Response.LastHop.Port)
  else begin
//   raise Exception.Create('We''ve not got a DNS A/AAA lookup thing yet');
  end;
end;

//* TIdSipAbstractLocator Protected methods ************************************

procedure TIdSipAbstractLocator.AddUriLocation(AddressOfRecord: TIdSipUri;
                                               List: TIdSipLocations);
var
  UriLocation: TIdSipLocation;
begin
  UriLocation := Self.CreateLocationFromUri(AddressOfRecord);
  try
  List.AddLocation(UriLocation.Transport,
                   UriLocation.Address,
                   UriLocation.Port);
  finally
    UriLocation.Free;
  end;
end;

function TIdSipAbstractLocator.CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
var
  Address:   String;
  Port:      Cardinal;
  Transport: String;
begin
  if AddressOfRecord.HasParameter(TransportParam) then begin
    Transport := ParamToTransport(AddressOfRecord.Transport);
  end
  else begin
    if (AddressOfRecord.Scheme = SipsScheme) then
      Transport := TlsTransport
    else
      Transport := UdpTransport;
  end;

  Address := AddressOfRecord.Host;
  Port    := AddressOfRecord.Port;

  Result := TIdSipLocation.Create(Transport, Address, Port);
end;

//******************************************************************************
//* TIdSipLocator                                                              *
//******************************************************************************
//* TIdSipLocator Public methods ***********************************************

function TIdSipLocator.ResolveNameRecords(const DomainName: String): TStrings;
begin
  raise Exception.Create('TIdSipLocator doesn''t know how to ResolveNameRecords');
end;

function TIdSipLocator.ResolveNAPTR(const DomainName: String): TIdNaptrRecords;
begin
  raise Exception.Create('TIdSipLocator doesn''t know how to ResolveNAPTR');
end;

//******************************************************************************
//* TIdDomainNameRecord                                                        *
//******************************************************************************
//* TIdDomainNameRecord Public methods *****************************************

constructor TIdDomainNameRecord.Create(const RecordType, Domain, IPAddress: String);
begin
  inherited Create;

  Self.fDomain     := Domain;
  Self.fIPAddress  := IPAddress;
  Self.fRecordType := RecordType;
end;

function TIdDomainNameRecord.Copy: TIdDomainNameRecord;
begin
  Result := TIdDomainNameRecord.Create(Self.RecordType,
                                       Self.Domain,
                                       Self.IPAddress);
end;

//******************************************************************************
//* TIdNaptrRecord                                                             *
//******************************************************************************
//* TIdNaptrRecord Public methods **********************************************

constructor TIdNaptrRecord.Create(const Key: String;
                                  Order: Word;
                                  Preference: Word;
                                  const Flags: String;
                                  const Service: String;
                                  const Regex: String;
                                  const Value: String);
begin
  inherited Create;

  Self.fKey        := Key;
  Self.fOrder      := Order;
  Self.fPreference := Preference;
  Self.fFlags      := Flags;
  Self.fService    := Service;
  Self.fRegex      := Regex;
  Self.fValue      := Value;
end;

function TIdNaptrRecord.Copy: TIdNaptrRecord;
begin
  Result := TIdNaptrRecord.Create(Self.Key,
                                  Self.Order,
                                  Self.Preference,
                                  Self.Flags,
                                  Self.Service,
                                  Self.Regex,
                                  Self.Value);
end;

//******************************************************************************
//* TIdNaptrRecords                                                            *
//******************************************************************************
//* TIdNaptrRecords Public methods *********************************************

constructor TIdNaptrRecords.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdNaptrRecords.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdNaptrRecords.Add(Copy: TIdNaptrRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdNaptrRecords.Clear;
begin
  Self.List.Clear;
end;

function TIdNaptrRecords.Count: Integer;
begin
  Result := Self.List.Count;
end;

//* TIdNaptrRecords Private methods ********************************************

function TIdNaptrRecords.GetItems(Index: Integer): TIdNaptrRecord;
begin
  Result := Self.List[Index] as TIdNaptrRecord;
end;

end.
