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

  TIdBaseList = class(TObject)
  private
    fList: TObjectList;
  protected
    property List: TObjectList read fList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    function  Count: Integer;
    procedure Delete(Index: Integer);
    function  IsEmpty: Boolean;
  end;

  TIdSipLocations = class(TIdBaseList)
  private
    function GetLocation(Index: Integer): TIdSipLocation;
  public
    procedure AddLocation(const Transport: String;
                          const Address: String;
                          Port: Cardinal);
    function  First: TIdSipLocation;

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

  TIdNaptrRecord = class;
  TIdNaptrRecords = class;
  TIdSrvRecords = class;

  // Given a SIP or SIPS URI, I return (using FindServersFor) a set of tuples of
  // the form (transport, IP address, port) that you can use to send a SIP
  // message.
  TIdSipAbstractLocator = class(TObject)
  private
    procedure ClearOutUnwantedNaptrRecords(TargetUri: TIdUri;
                                           Recs: TIdNaptrRecords);
    procedure ClearOutUnwantedSrvRecords(Recs: TIdSrvRecords);
    function  PassNaptrFiltering(TargetUri: TIdUri;
                                 NAPTR: TIdNaptrRecord): Boolean;
  protected
    procedure AddUriLocation(AddressOfRecord: TIdSipUri;
                             List: TIdSipLocations);
    function  CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
    procedure PerformNameLookup(const DomainName: String;
                                Result: TStrings); virtual;
    procedure PerformNAPTRLookup(TargetUri: TIdUri;
                                 Result: TIdNaptrRecords); virtual;
    procedure PerformSRVLookup(const ServiceAndDomain: String;
                               Result: TIdSrvRecords); virtual;
  public
    constructor Create; virtual;

    function  FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; overload;
    function  FindServersFor(const AddressOfRecord: String): TIdSipLocations; overload;
    function  FindServersFor(Response: TIdSipResponse): TIdSipLocations; overload;
    procedure ResolveNameRecords(const DomainName: String;
                                 Result: TStrings); virtual;
    procedure ResolveNAPTR(TargetUri: TIdUri;
                           Result: TIdNaptrRecords);
    procedure ResolveSRV(const ServiceAndDomain: String;
                         Result: TIdSrvRecords);
    function  SrvTarget(UsingSips: Boolean;
                        const Protocol: String;
                        const Domain: String): String;
    function  TransportFor(AddressOfRecord: TIdSipUri;
                           NAPTR: TIdNaptrRecords;
                           SRV: TIdSrvRecords;
                           AtypeRecords: TStrings): String;
  end;

  // I take an address-of-record SIP/SIPS URI and return a URL at which
  // a server can take a call for the given address-of-record.
  TIdSipLocator = class(TIdSipAbstractLocator)
  end;

  TIdDomainNameRecord = class(TObject)
  private
    fDomain:     String;
    fIPAddress:  String;
    fRecordType: String;
  public
    constructor Create(const RecordType: String;
                       const Domain: String;
                       const IPAddress: String);

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
  TIdNaptrRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdNaptrRecord;
  public
    procedure Add(Copy: TIdNaptrRecord); overload;
    procedure Add(const Key: String;
                  Order: Word;
                  Preference: Word;
                  const Flags: String;
                  const Service: String;
                  const Regex: String;
                  const Value: String); overload;
    procedure Sort;

    property Items[Index: Integer]: TIdNaptrRecord read GetItems; default;
  end;

  // RFC 2782 defines SRV (service) records.
  TIdSrvRecord = class(TObject)
  private
    fDomain:   String;
    fPort:     Cardinal;
    fPriority: Word;
    fService:  String;
    fTarget:   String;
    fWeight:   Word;
  public
    constructor Create(const Domain: String;
                       const Service: String;
                       Priority: Word;
                       Weight: Word;
                       Port: Cardinal;
                       const Target: String);

    function Copy: TIdSrvRecord;

    property Domain:   String   read fDomain;
    property Port:     Cardinal read fPort;
    property Priority: Word     read fPriority;
    property Service:  String   read fService;
    property Target:   String   read fTarget;
    property Weight:   Word     read fWeight;
  end;

  TIdSrvRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdSrvRecord;
  public
    procedure Add(Copy: TIdSrvRecord);
    procedure Sort;

    property Items[Index: Integer]: TIdSrvRecord read GetItems; default;
  end;

const
  DnsA6Record   = 'A6';
  DnsARecord    = 'A';
  DnsAAAARecord = 'AAAA';

// NAPTR (RFCs 2915, 3401-3) constants
const
  NaptrDelimiter   = '+';
  NaptrNullFlag    = '';
  NaptrSipService  = 'SIP';
  NaptrSipsService = 'SIPS';
  SipSctpService   = NaptrSipService  + NaptrDelimiter + 'D2S';
  SipsTlsService   = NaptrSipsService + NaptrDelimiter + 'D2T';
  SipTcpService    = NaptrSipService  + NaptrDelimiter + 'D2T';
  SipUdpService    = NaptrSipService  + NaptrDelimiter + 'D2U';

// SRV (RFC 2782) constants
const
  SrvNotAvailableTarget = '.';
  SrvSctpPrefix         = '_sip._sctp';
  SrvSipService         = 'sip';
  SrvSipsService        = 'sips';
  SrvTcpPrefix          = '_sip._tcp';
  SrvUdpPrefix          = '_sip._udp';
  SrvTlsPrefix          = '_sips._tcp';

function NaptrSort(Item1, Item2: Pointer): Integer;
function ServiceToTransport(const NaptrService: String): String;
function SrvSort(Item1, Item2: Pointer): Integer;

implementation

uses
  IdGlobal, IdSimpleParser, SysUtils;

//******************************************************************************
//* Unit functions & procedures                                                *
//******************************************************************************
//* Unit Public functions & procedures *****************************************

function NaptrSort(Item1, Item2: Pointer): Integer;
var
  A: TIdNaptrRecord;
  B: TIdNaptrRecord;
begin
  // Result < 0 if Item1 is less than Item2,
  // Result = 0 if they are equal, and
  // Result > 0 if Item1 is greater than Item2.

  // Keys equal?
  // Order equal?
  // Service equal?
  // Preference equal?

  A := TIdNaptrRecord(Item1);
  B := TIdNaptrRecord(Item2);

  if (A.Key < B.Key) then
    Result := -1
  else if (A.Key > B.Key) then
    Result := 1
  else
    Result := 0;

  if (Result = 0) then
    Result := A.Order - B.Order;

  if (Result = 0) then begin
    if (A.Service < B.Service) then
      Result := -1
    else if (A.Service > B.Service) then
      Result := 1
    else
      Result := 0;
  end;

  if (Result = 0) then
    Result := A.Preference - B.Preference;
end;

function ServiceToTransport(const NaptrService: String): String;
begin
  // TODO: We really need to reference a transport registry of some kind.
  if      IsEqual(NaptrService, SipsTlsService) then
    Result := TlsTransport
  else if IsEqual(NaptrService, SipTcpService) then
    Result := TcpTransport
  else if IsEqual(NaptrService, SipUdpService) then
    Result := UdpTransport
  else if IsEqual(NaptrService, SipSctpService) then
    Result := SctpTransport
  else
    raise Exception.Create('Don''t know what transport to use for a NAPTR service ''' + NaptrService + '''');
end;

function SrvSort(Item1, Item2: Pointer): Integer;
var
  A: TIdSrvRecord;
  B: TIdSrvRecord;
begin
  // Result < 0 if Item1 is less than Item2,
  // Result = 0 if they are equal, and
  // Result > 0 if Item1 is greater than Item2.

  A := TIdSrvRecord(Item1);
  B := TIdSrvRecord(Item2);

  Result := A.Priority - B.Priority;

  // Lower weight = more often used
  if (Result = 0) then
    Result := B.Weight - A.Weight;
end;

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
//* TIdBaseList                                                                *
//******************************************************************************
//* TIdBaseList Public methods *************************************************

constructor TIdBaseList.Create;
begin
  inherited Create;

  Self.fList := TObjectList.Create(true);
end;

destructor TIdBaseList.Destroy;
begin
  Self.fList.Free;

  inherited Destroy;
end;

procedure TIdBaseList.Clear;
begin
  Self.List.Clear;
end;

function TIdBaseList.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdBaseList.Delete(Index: Integer);
begin
  Self.List.Delete(Index);
end;

function TIdBaseList.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

procedure TIdSipLocations.AddLocation(const Transport: String;
                                      const Address: String;
                                      Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Add(NewLocation);
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  Result := Self.Items[0];
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
  ARecords:  TStrings;
  Naptr:     TIdNaptrRecords;
  Srv:       TIdSrvRecords;
  Target:    String;
  Transport: String;
begin
  Result := TIdSipLocations.Create;

  Naptr := TIdNaptrRecords.Create;
  try
    Srv := TIdSrvRecords.Create;
    try
      ARecords := TStringList.Create;

      Transport := Self.TransportFor(AddressOfRecord, Naptr, Srv, ARecords);
      try
        if AddressOfRecord.HasMaddr then
          Target := AddressOfRecord.Maddr
        else
          Target := AddressOfRecord.Host;

        if TIdIPAddressParser.IsNumericAddress(Target) then begin
          Result.AddLocation(Transport, Target, AddressOfRecord.Port);
          Exit;
        end;

        Self.ResolveNAPTR(AddressOfRecord, Naptr);

        Self.ResolveSRV(Naptr[0].Value, Srv);
      finally
        ARecords.Free;
      end;
    finally
      Srv.Free;
    end;
  finally
    Naptr.Free;
  end;
  raise Exception.Create('No SRV lookups yet');
  raise Exception.Create('No A/AAAA/A6 lookups yet');                          
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
  or TIdIPAddressParser.IsIPv6Reference(Response.LastHop.SentBy) then
    Result.AddLocation(Response.LastHop.Transport,
                       Response.LastHop.SentBy,
                       Response.LastHop.Port)
  else begin
    raise Exception.Create('We''ve not got a DNS A/AAAA lookup thing yet');
  end;
end;

procedure TIdSipAbstractLocator.ResolveNameRecords(const DomainName: String;
                                                   Result: TStrings);
begin
  // My subclasses perform a DNS lookup for A/AAAA/A6 records.

  Self.PerformNameLookup(DomainName, Result);
end;

procedure TIdSipAbstractLocator.ResolveNAPTR(TargetUri: TIdUri;
                                             Result: TIdNaptrRecords);
begin
  // My subclasses perform a DNS lookup for NAPTR records. Specifically, those
  // NAPTR RRs for services SIP+D2T, SIP+D2U, SIP+D2S, etc. - all the services
  // for all the transports this stack supports.

  Self.PerformNAPTRLookup(TargetUri, Result);
  Self.ClearOutUnwantedNaptrRecords(TargetUri, Result);
  Result.Sort;
end;

procedure TIdSipAbstractLocator.ResolveSRV(const ServiceAndDomain: String;
                                           Result: TIdSrvRecords);
begin
  // My subclasses perform a DNS lookup for SRV records. ServiceAndDomain
  // typically looks something like "_sips._tcp.example.com".

  Self.PerformSRVLookup(ServiceAndDomain, Result);
  Self.ClearOutUnwantedSrvRecords(Result);
  Result.Sort;
end;

function TIdSipAbstractLocator.SrvTarget(UsingSips: Boolean;
                                         const Protocol: String;
                                         const Domain: String): String;
var
  Service: String;
begin
  Result := '_';

  if UsingSips then
    Service := SrvSipsService
  else
    Service := SrvSipService;

  Result := '_' + Service + '._' + Lowercase(Protocol) + '.' + Domain;
end;

function TIdSipAbstractLocator.TransportFor(AddressOfRecord: TIdSipUri;
                                            NAPTR: TIdNaptrRecords;
                                            SRV: TIdSrvRecords;
                                            AtypeRecords: TStrings): String;
var
  Target: String;
begin
  if AddressOfRecord.HasMaddr then
    Target := AddressOfRecord.Maddr
  else
    Target := AddressOfRecord.Host;

  if AddressOfRecord.TransportIsSpecified then begin
    Result := ParamToTransport(AddressOfRecord.Transport);
    Exit;
  end
  else if TIdIPAddressParser.IsNumericAddress(Target)
       or AddressOfRecord.PortIsSpecified then begin
    Result := ParamToTransport(AddressOfRecord.Transport);
    Exit;
  end;

  Self.ResolveNAPTR(AddressOfRecord, NAPTR);

  if NAPTR.IsEmpty then begin
    raise Exception.Create('Look up what to do when there''re no NAPTR records')
    // SRV queries on each transport this stack supports.
    // If none, use TCP (i.e., TLS) for a SIPS URI, or UDP otherwise
  end
  else
    Result := ServiceToTransport(NAPTR[0].Service)
{
   First, a client resolving a SIPS URI MUST discard any services that
   do not contain "SIPS" as the protocol in the service field.  The
   converse is not true, however.  A client resolving a SIP URI SHOULD
   retain records with "SIPS" as the protocol, if the client supports
   TLS.  Second, a client MUST discard any service fields that identify
   a resolution service whose value is not "D2X", for values of X that
   indicate transport protocols supported by the client.  The NAPTR
   processing as described in RFC 2915 will result in the discovery of
   the most preferred transport protocol of the server that is supported
   by the client, as well as an SRV record for the server.  It will also
   allow the client to discover if TLS is available and its preference
   for its usage.
}
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

procedure TIdSipAbstractLocator.PerformNameLookup(const DomainName: String;
                                                  Result: TStrings);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformNameLookup');
end;

procedure TIdSipAbstractLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                                  Result: TIdNaptrRecords);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformNAPTRLookup');
end;

procedure TIdSipAbstractLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                                 Result: TIdSrvRecords);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformSRVLookup');
end;

//* TIdSipAbstractLocator Protected methods ************************************

procedure TIdSipAbstractLocator.ClearOutUnwantedNaptrRecords(TargetUri: TIdUri;
                                                             Recs: TIdNaptrRecords);
var
  I: Integer;
begin
  I := 0;
  while (I < Recs.Count) do begin
    if Self.PassNaptrFiltering(TargetUri, Recs[I]) then
      Inc(I)
    else
      Recs.Delete(I);
  end;
end;

procedure TIdSipAbstractLocator.ClearOutUnwantedSrvRecords(Recs: TIdSrvRecords);
begin
end;

function TIdSipAbstractLocator.PassNaptrFiltering(TargetUri: TIdUri;
                                                  NAPTR: TIdNaptrRecord): Boolean;
var
  Protocol:          String;
  ServiceResolution: String;
begin
  // Return true if the NAPTR service is something like "SIP+D2X" or "SIPS+D2X"
  // and false otherwise.
  // Note that if TargetUri.IsSipsUri we must return false for any non-SIPS
  // ServiceResolutions.

  Protocol          := Naptr.Service;
  ServiceResolution := Fetch(Protocol, NaptrDelimiter);

  if TargetUri.IsSipsUri then
    Result := (ServiceResolution = NaptrSipsService)
  else 
    Result := (ServiceResolution = NaptrSipService)
           or (ServiceResolution = NaptrSipsService);

  Result := Result
        and (Length(Protocol) > 2)
        and (Copy(Protocol, 1, 2) = 'D2');
end;

//******************************************************************************
//* TIdSipLocator                                                              *
//******************************************************************************
//* TIdSipLocator Public methods ***********************************************

//******************************************************************************
//* TIdDomainNameRecord                                                        *
//******************************************************************************
//* TIdDomainNameRecord Public methods *****************************************

constructor TIdDomainNameRecord.Create(const RecordType: String;
                                       const Domain: String;
                                       const IPAddress: String);
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

procedure TIdNaptrRecords.Add(Copy: TIdNaptrRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdNaptrRecords.Add(const Key: String;
                              Order: Word;
                              Preference: Word;
                              const Flags: String;
                              const Service: String;
                              const Regex: String;
                              const Value: String);
var
  NewRec: TIdNaptrRecord;
begin
  NewRec := TIdNaptrRecord.Create(Key,
                                  Order,
                                  Preference,
                                  Flags,
                                  Service,
                                  Regex,
                                  Value);
  Self.List.Add(NewRec);
end;

procedure TIdNaptrRecords.Sort;
begin
  Self.List.Sort(NaptrSort);
end;

//* TIdNaptrRecords Private methods ********************************************

function TIdNaptrRecords.GetItems(Index: Integer): TIdNaptrRecord;
begin
  Result := Self.List[Index] as TIdNaptrRecord;
end;

//******************************************************************************
//* TIdSrvRecord                                                               *
//******************************************************************************
//* TIdSrvRecord Public methods ************************************************

constructor TIdSrvRecord.Create(const Domain: String;
                                const Service: String;
                                Priority: Word;
                                Weight: Word;
                                Port: Cardinal;
                                const Target: String);
begin
  inherited Create;

  Self.fDomain   := Domain;
  Self.fPort     := Port;
  Self.fPriority := Priority;
  Self.fService  := Service;
  Self.fTarget   := Target;
  Self.fWeight   := Weight;
end;

function TIdSrvRecord.Copy: TIdSrvRecord;
begin
  Result := TIdSrvRecord.Create(Self.Domain,
                                Self.Service,
                                Self.Priority,
                                Self.Weight,
                                Self.Port,
                                Self.Target);
end;

//******************************************************************************
//* TIdSrvRecords                                                              *
//******************************************************************************
//* TIdSrvRecords Public methods ***********************************************

procedure TIdSrvRecords.Add(Copy: TIdSrvRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdSrvRecords.Sort;
begin
  Self.List.Sort(SrvSort);
end;

//* TIdSrvRecords Private methods **********************************************

function TIdSrvRecords.GetItems(Index: Integer): TIdSrvRecord;
begin
  Result := Self.List[Index] as TIdSrvRecord;
end;

end.
