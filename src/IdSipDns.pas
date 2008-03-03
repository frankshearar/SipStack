{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipDns;

interface

uses
  Classes, Contnrs;

type
  TIdResourceRecord = class(TObject)
  public
    function AsString: String; virtual;
    function ResourceType: String; virtual;
  end;

  TIdBaseList = class(TObject)
  private
    fList: TObjectList;

    function GetRR(Index: Integer): TIdResourceRecord;
  protected
    property List: TObjectList read fList;
    property RR[Index: Integer]: TIdResourceRecord read GetRR;
  public
    constructor Create;
    destructor  Destroy; override;

    function  AsString: String;
    procedure Clear;
    function  Count: Integer;
    procedure Delete(Index: Integer);
    function  IsEmpty: Boolean;
  end;

  // I represent a name record of some sort. I might contain an A (RFC 1034) or
  // AAAA (RFC 1886) records, or some future name record (like, perhaps, A6
  // (RFC 2874).
  //
  // As you can see, I am a Value Object.
  TIdDomainNameRecord = class(TIdResourceRecord)
  private
    fDomain:     String;
    fIPAddress:  String;
    fRecordType: String;
  public
    constructor Create(const RecordType: String;
                       const Domain: String;
                       const IPAddress: String);

    function AsString: String; override;
    function Copy: TIdDomainNameRecord;
    function ResourceType: String; override;

    property Domain:     String read fDomain;
    property IPAddress:  String read fIPAddress;
    property RecordType: String read fRecordType;
  end;

  // I provide a collection of name records.
  TIdDomainNameRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdDomainNameRecord;
  public
    procedure Add(Copy: TIdDomainNameRecord); overload;
    procedure Add(const RecordType: String;
                  const Domain: String;
                  const IPAddress: String); overload;
    function  Copy: TIdDomainNameRecords;
    procedure Sort;

    property Items[Index: Integer]: TIdDomainNameRecord read GetItems; default;
  end;

  // I represent a CNAME record.
  TIdDomainNameAliasRecord = class(TIdResourceRecord)
  private
    fAlias:         String;
    fCanonicalName: String;
  public
    constructor Create(const CanonicalName: String;
                       const Alias: String);

    function AsString: String; override;
    function Copy: TIdDomainNameAliasRecord;
    function ResourceType: String; override;

    property Alias:         String read fAlias;
    property CanonicalName: String read fCanonicalName;
  end;

  // I provide a collection of name record aliases.
  TIdDomainNameAliasRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdDomainNameAliasRecord;
  public
    procedure Add(Copy: TIdDomainNameAliasRecord); overload;
    procedure Add(const CanonicalName: String;
                  const Alias: String); overload;
    function  Copy: TIdDomainNameAliasRecords;
    procedure Sort;

    property Items[Index: Integer]: TIdDomainNameAliasRecord read GetItems; default;
  end;

  TIdSrvRecords = class;

  // RFC 3403 obsoletes RFC 2915, and defines NAPTR records as part of the
  // Dynamic Delegation Discovery System (RFCs 3401-3405). RFC 3403 says that
  // domain name servers MAY also return associated records with the results of
  // a NAPTR lookup - A, SRV records, for instance.
  //
  // I represent a single NAPTR record. As you can see, I am a Value Object.
  //
  // My Value property gets its name from RFC 3401's nomenclature. in "classic
  // NAPTR" language it would be called "Replacement".
  //
  // Note that while I aspire to be a Value Object, my ServiceRecords property
  // is actually mutable. This might seem odd, but remember that the
  // ServiceRecords have no bearing on my NAPTR RR - they're included as a
  // convenience, a denormalisation for efficiency, if you like.
  TIdNaptrRecord = class(TIdResourceRecord)
  private
    fFlags:          String;
    fKey:            String; // The DDDS key
    fOrder:          Word;
    fPreference:     Word;
    fRegex:          String;
    fService:        String;
    fServiceRecords: TIdSrvRecords;
    fValue:          String; // The DDDS value (a domain name we can feed into SRV queries)
  public
    constructor Create(const Key: String;
                       Order: Word;
                       Preference: Word;
                       const Flags: String;
                       const Service: String;
                       const Regex: String;
                       const Value: String);
    destructor Destroy; override;

    function AsSipTransport: String;
    function AsString: String; override;
    function Copy: TIdNaptrRecord;
    function IsSecureService: Boolean;
    function ResourceType: String; override;

    property Flags:          String        read fFlags;
    property Key:            String        read fKey;
    property Order:          Word          read fOrder;
    property Preference:     Word          read fPreference;
    property Regex:          String        read fRegex;
    property Service:        String        read fService;
    property ServiceRecords: TIdSrvRecords read fServiceRecords;
    property Value:          String        read fValue;
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
    function  AnyAppropriateRecord(Transports: TStrings): TIdNaptrRecord;
    function  RecordFor(ReplacementValue: String): TIdNaptrRecord;
    procedure Sort;

    property Items[Index: Integer]: TIdNaptrRecord read GetItems; default;
  end;

  // RFC 2782 defines SRV (service) records.
  // RFC 2782 says that name servers supporting SRV RRs SHOULD return all
  // A/AAAA records for the targets of each SRV RR. We support this with the
  // NameRecords property.
  //
  // Note that while I aspire to be a Value Object, my NameRecords property
  // is actually mutable. This might seem odd, but remember that the NameRecords
  // have no bearing on my SRV RR - they're included as a convenience, a
  // denormalisation for efficiency, if you like.
  TIdSrvRecord = class(TIdResourceRecord)
  private
    fDomain:      String;
    fNameRecords: TIdDomainNameRecords;
    fPort:        Cardinal;
    fPriority:    Word;
    fService:     String;
    fTarget:      String;
    fWeight:      Word;
  public
    constructor Create(const Domain: String;
                       const Service: String;
                       Priority: Word;
                       Weight: Word;
                       Port: Cardinal;
                       const Target: String);
    destructor  Destroy; override;

    function AsString: String; override;
    function Copy: TIdSrvRecord;
    function QueryName: String;
    function ResourceType: String; override;
    function SipTransport: String;

    property Domain:      String               read fDomain;
    property NameRecords: TIdDomainNameRecords read fNameRecords;
    property Port:        Cardinal             read fPort;
    property Priority:    Word                 read fPriority;
    property Service:     String               read fService;
    property Target:      String               read fTarget;
    property Weight:      Word                 read fWeight;
  end;

  TIdSrvRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdSrvRecord;
    function ItemFor(const Target: String): TIdSrvRecord;
  public
    class function TransportToPrefix(const Transport: String): String;
    function  Add(Copy: TIdSrvRecord): TIdSrvRecord; overload;
    function  Add(const Domain: String;
                  const Service: String;
                  Priority: Word;
                  Weight: Word;
                  Port: Cardinal;
                  const Target: String): TIdSrvRecord; overload;
    procedure AddNameRecord(const RecordType: String;
                            const Domain: String;
                            const IPAddress: String);
    procedure AddServiceRecords(SRVs: TIdSrvRecords);
    function  Last: TIdSrvRecord;
    procedure Sort;

    property Items[Index: Integer]: TIdSrvRecord read GetItems; default;
  end;

  // DNS servers typically return several kinds of resource records in the
  // answer to a query. For instance, a NAPTR lookup can return all of SRV,
  // A, AAAA, CNAME records. I thus can contain a miscellany of resource
  // records, and supply methods of searching for relationships between the
  // resource records.
  TIdResourceRecords = class(TIdBaseList)
  private
    function GetItems(Index: Integer): TIdResourceRecord;
  public
    procedure AddARecord(Domain,
                         IPAddress: String);
    procedure AddAAAARecord(Domain,
                             IPAddress: String);
    procedure AddCNAMERecord(CanonicalName,
                             Alias: String);
    procedure AddNAPTRRecord(Key: String;
                             Order: Word;
                             Preference: Word;
                             Flags,
                             Service,
                             Regex,
                             Value: String);
    procedure AddSRVRecord(Domain,
                           Service: String;
                           Priority: Word;
                           Weight: Word;
                           Port: Cardinal;
                           Target: String);
    procedure CollectAliases(Result: TIdDomainNameAliasRecords);
    procedure CollectNamePointerRecords(Result: TIdNaptrRecords);
    procedure CollectNameRecords(Result: TIdDomainNameRecords);
    procedure CollectServiceRecords(Result: TIdSrvRecords);
    function  ContainsType(ResourceRecordType: String): Boolean;
    function  LastRecord: TIdResourceRecord;

    property Items[Index: Integer]: TIdResourceRecord read GetItems; default;
  end;

const
  DnsARecord     = 'A';
  DnsAAAARecord  = 'AAAA';
  DnsCNAMERecord = 'CNAME';
  DnsNAPTRRecord = 'NAPTR';
  DnsSRVRecord   = 'SRV';

// NAPTR (RFCs 2915, 3401-3) constants
// Example full services: 'SIP+D2U', 'SIPS+D2S'
const
  NaptrDelimiter          = '+';
  NaptrDeliverTo          = 'D2';
  NaptrServiceMiddleToken = NaptrDelimiter + NaptrDeliverTo;

  NaptrDefaultFlags      = 's';
  NaptrNullFlag          = '';
  NaptrSipService        = 'SIP';
  NaptrSipsService       = 'SIPS';
  NaptrSctpTransport     = 'S';
  NaptrTcpTransport      = 'T';
  NaptrUdpTransport      = 'U';

  NaptrSctpService        = NaptrSipService  + NaptrServiceMiddleToken + NaptrSctpTransport;
  NaptrTcpService         = NaptrSipService  + NaptrServiceMiddleToken + NaptrTcpTransport;
  NaptrTlsService         = NaptrSipsService + NaptrServiceMiddleToken + NaptrTcpTransport;
  NaptrTlsOverSctpService = NaptrSipsService + NaptrServiceMiddleToken + NaptrSctpTransport;
  NaptrUdpService         = NaptrSipService  + NaptrServiceMiddleToken + NaptrUdpTransport;

// SRV (RFC 2782) constants
const
  SrvNotAvailableTarget = '.';
  SrvSctpPrefix         = '_sip._sctp';
  SrvSipService         = 'sip';
  SrvSipsService        = 'sips';
  SrvTcpPrefix          = '_sip._tcp';
  SrvTlsPrefix          = '_sips._tcp';
  SrvTlsOverSctpPrefix  = '_sips._sctp';
  SrvUdpPrefix          = '_sip._udp';

function AliasSort(Item1, Item2: Pointer): Integer;
function DomainNameSort(Item1, Item2: Pointer): Integer;
function NaptrSort(Item1, Item2: Pointer): Integer;
function SrvSort(Item1, Item2: Pointer): Integer;

implementation

uses
  IdSimpleParser, IdSipMessage, RuntimeSafety, SysUtils;

const
  ItemNotFoundIndex = -1;
  LessThan          = -1;
  GreaterThan       = 1;
  Equal             = 0;

//******************************************************************************
//* Unit functions & procedures                                                *
//******************************************************************************
//* Unit Private functions & procedures ****************************************

function CompareIPv4Address(Addr1, Addr2: String): Integer;
var
  Inet1: Cardinal;
  Inet2: Cardinal;
begin
  // Considering the IPv4 addresses as 32 bit numbers, order the addresses in
  // ascending order.

  Inet1 := TIdIPAddressParser.InetAddr(Addr1);
  Inet2 := TIdIPAddressParser.InetAddr(Addr2);

  if (Inet1 < Inet2) then
    Result := LessThan
  else if (Inet1 > Inet2) then
    Result := GreaterThan
  else
    Result := Equal;
end;

function CompareIPv6Address(Addr1, Addr2: String): Integer;
var
  I:    Integer;
  RecA: TIdIPv6AddressRec;
  RecB: TIdIPv6AddressRec;
begin
  // Considering the IPv6 addresses as 128 bit numbers, order the addresses in
  // ascending order.

  TIdIPAddressParser.ParseIPv6Address(Addr1, RecA);
  TIdIPAddressParser.ParseIPv6Address(Addr2, RecB);

  Result := Equal;
  for I := Low(RecA) to High(RecB) do begin
    if (RecA[I] < RecB[I]) then
      Result := LessThan
    else if (RecA[I] > RecB[I]) then
      Result := GreaterThan;

    if (Result <> Equal) then Break;
  end;
end;

function CompareRecordType(Item1, Item2: String): Integer;
begin
  // We cheat: "A" < "AAAA", so we just reverse the string sort order.

  Result := CompareStr(Item2, Item1);
end;

function CompareAddress(Item1, Item2: String): Integer;
var
  Ver1: TIdIPVersion;
  Ver2: TIdIPVersion;
begin
  Ver1 := TIdIPAddressParser.IPVersion(Item1);
  Ver2 := TIdIPAddressParser.IPVersion(Item2);

  if (Ver1 <> Ver2) then begin
    if (Ver1 = Id_IPv6) then
      Result := LessThan
    else
      Result := GreaterThan;
    Exit;
  end;

  if (Ver1 = Id_IPv4) then
    Result := CompareIPv4Address(Item1, Item2)
  else if (Ver2 = Id_IPv6) then
    Result := CompareIPv6Address(Item1, Item2)
  else begin
    // We should never reach here, but if we do, just say "the two addresses
    // are equal in order".
    Result := Equal;
  end;
end;

function CompareString(Item1, Item2: String): Integer;
begin
  if (Item1 < Item2) then
    Result := LessThan
  else if (Item1 > Item2) then
    Result := GreaterThan
  else
    Result := Equal;
end;

function IsEqual(S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
end;

//* Unit Public functions & procedures *****************************************

function AliasSort(Item1, Item2: Pointer): Integer;
var
  A: TIdDomainNameAliasRecord;
  B: TIdDomainNameAliasRecord;
begin
  A := TIdDomainNameAliasRecord(Item1);
  B := TIdDomainNameAliasRecord(Item2);

  if (A.CanonicalName < B.CanonicalName) then
    Result := LessThan
  else if (A.CanonicalName > B.CanonicalName) then
    Result := GreaterThan
  else
    Result := Equal;

  if (Result = Equal) then begin
    if (A.Alias < B.Alias) then
      Result := LessThan
    else if (A.Alias > B.Alias) then
      Result := GreaterThan;
  end;
end;

function DomainNameSort(Item1, Item2: Pointer): Integer;
var
  A: TIdDomainNameRecord;
  B: TIdDomainNameRecord;
begin
  // AAAA records first
  // Then in alphabetic order of domain name
  // Then in descending (numeric) order of address

  A := TIdDomainNameRecord(Item1);
  B := TIdDomainNameRecord(Item2);

  Result := CompareRecordType(A.RecordType, B.RecordType);

  if (Result = Equal) then begin
    Result := CompareString(A.Domain, B.Domain);

    if (Result = Equal) then
      Result := CompareAddress(A.IPAddress, B.IPAddress);
  end;
end;

function NaptrSort(Item1, Item2: Pointer): Integer;
var
  A:         TIdNaptrRecord;
  B:         TIdNaptrRecord;
  AIsSecure: Boolean;
  BIsSecure: Boolean;
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
    Result := LessThan
  else if (A.Key > B.Key) then
    Result := GreaterThan
  else
    Result := Equal;

  if (Result = Equal) then
    Result := A.Order - B.Order;

  if (Result = Equal) then begin
    // If the A and B have the Protocol, prefer the one that
    // uses a secure Transport. Thus,
    //   SIP+D2T < SIP+D2U;
    //   SIPS+D2T < SIP+D2T

    AIsSecure := A.IsSecureService;
    BIsSecure := B.IsSecureService;

    if AIsSecure xor BIsSecure then begin
      if AIsSecure then
        Result := LessThan
      else
        Result := GreaterThan;
    end else begin
      if (A.Service < B.Service) then
        Result := LessThan
      else if (A.Service > B.Service) then
        Result := GreaterThan;
    end;
  end;

  if (Result = Equal) then
    Result := A.Preference - B.Preference;
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

  // TODO: This weight algorithm's actually wrong. See RFC 2782:
  //      In the absence of a protocol whose specification calls for the
  //      use of other weighting information, a client arranges the SRV
  //      RRs of the same Priority in the order in which target hosts,
  //      specified by the SRV RRs, will be contacted. The following
  //      algorithm SHOULD be used to order the SRV RRs of the same
  //      priority:

  //      To select a target to be contacted next, arrange all SRV RRs
  //      (that have not been ordered yet) in any order, except that all
  //      those with weight 0 are placed at the beginning of the list.
  //
  //      Compute the sum of the weights of those RRs, and with each RR
  //      associate the running sum in the selected order. Then choose a
  //      uniform random number between 0 and the sum computed
  //      (inclusive), and select the RR whose running sum value is the
  //      first in the selected order which is greater than or equal to
  //      the random number selected. The target host specified in the
  //      selected SRV RR is the next one to be contacted by the client.
  //      Remove this SRV RR from the set of the unordered SRV RRs and
  //      apply the described algorithm to the unordered SRV RRs to select
  //      the next target host.  Continue the ordering process until there
  //      are no unordered SRV RRs.  This process is repeated for each
  //      Priority.

  // Lower weight = less often used
  if (Result = 0) then
    Result := B.Weight - A.Weight;
end;

//******************************************************************************
//* TIdResourceRecord                                                          *
//******************************************************************************
//* TIdResourceRecord Public methods *******************************************

function TIdResourceRecord.AsString: String;
begin
  RaiseAbstractError(Self.ClassName, 'AsString');
end;

function TIdResourceRecord.ResourceType: String;
begin
  RaiseAbstractError(Self.ClassName, 'ResourceType');
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

function TIdBaseList.AsString: String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count - 1 do
    Result := Result + Self.RR[I].AsString + CRLF;

  Result := Trim(Result);
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

//* TIdBaseList Private methods ************************************************

function TIdBaseList.GetRR(Index: Integer): TIdResourceRecord;
begin
  Result := Self.List[Index] as TIdResourceRecord;
end;

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

function TIdDomainNameRecord.AsString: String;
begin
  // Display Self's data in BIND format.
  Result := Format('%s %s %s', [Self.Domain, Self.ResourceType, Self.IPAddress]);
end;

function TIdDomainNameRecord.Copy: TIdDomainNameRecord;
begin
  Result := TIdDomainNameRecord.Create(Self.RecordType,
                                       Self.Domain,
                                       Self.IPAddress);
end;

function TIdDomainNameRecord.ResourceType: String;
begin
  Result := Self.RecordType;
end;

//******************************************************************************
//* TIdDomainNameRecords                                                       *
//******************************************************************************
//* TIdDomainNameRecords Public methods ****************************************

procedure TIdDomainNameRecords.Add(Copy: TIdDomainNameRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdDomainNameRecords.Add(const RecordType: String;
                                   const Domain: String;
                                   const IPAddress: String);
var
  NewRec: TIdDomainNameRecord;
begin
  NewRec := TIdDomainNameRecord.Create(RecordType, Domain, IPAddress);
  Self.List.Add(NewRec);
end;

function TIdDomainNameRecords.Copy: TIdDomainNameRecords;
var
  I: Integer;
begin
  Result := TIdDomainNameRecords.Create;

  for I := 0 to Self.Count - 1 do
    Result.Add(Self[I]);
end;

procedure TIdDomainNameRecords.Sort;
begin
  Self.List.Sort(DomainNameSort);
end;

//* TIdDomainNameRecords Private methods ***************************************

function TIdDomainNameRecords.GetItems(Index: Integer): TIdDomainNameRecord;
begin
  Result := Self.List[Index] as TIdDomainNameRecord;
end;

//******************************************************************************
//* TIdDomainNameAliasRecord                                                   *
//******************************************************************************
//* TIdDomainNameAliasRecord Public methods ************************************

constructor TIdDomainNameAliasRecord.Create(const CanonicalName: String;
                                            const Alias: String);
begin
  inherited Create;

  Self.fAlias         := Alias;
  Self.fCanonicalName := CanonicalName;
end;

function TIdDomainNameAliasRecord.AsString: String;
begin
  // Display Self's data in BIND format.
  Result := Format('%s %s %s', [Self.Alias, Self.ResourceType, Self.CanonicalName]);
end;

function TIdDomainNameAliasRecord.Copy: TIdDomainNameAliasRecord;
begin
  Result := TIdDomainNameAliasRecord.Create(Self.CanonicalName,
                                            Self.Alias);
end;

function TIdDomainNameAliasRecord.ResourceType: String;
begin
  Result := DnsCNAMERecord;
end;

//******************************************************************************
//* TIdDomainNameAliasRecords                                                  *
//******************************************************************************
//* TIdDomainNameAliasRecords Public methods ***********************************

procedure TIdDomainNameAliasRecords.Add(Copy: TIdDomainNameAliasRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdDomainNameAliasRecords.Add(const CanonicalName: String;
                                        const Alias: String);
var
  NewRec: TIdDomainNameAliasRecord;
begin
  NewRec := TIdDomainNameAliasRecord.Create(CanonicalName, Alias);
  Self.List.Add(NewRec);
end;

function TIdDomainNameAliasRecords.Copy: TIdDomainNameAliasRecords;
var
  I: Integer;
begin
  Result := TIdDomainNameAliasRecords.Create;

  for I := 0 to Self.Count - 1 do
    Result.Add(Self[I]);
end;

procedure TIdDomainNameAliasRecords.Sort;
begin
  Self.List.Sort(AliasSort);
end;

//* TIdDomainNameAliasRecords Private methods **********************************

function TIdDomainNameAliasRecords.GetItems(Index: Integer): TIdDomainNameAliasRecord;
begin
  Result := Self.List[Index] as TIdDomainNameAliasRecord;
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

  Self.fServiceRecords := TIdSrvRecords.Create;

  Self.fKey        := Key;
  Self.fOrder      := Order;
  Self.fPreference := Preference;
  Self.fFlags      := Flags;
  Self.fService    := Service;
  Self.fRegex      := Regex;
  Self.fValue      := Value;
end;

destructor TIdNaptrRecord.Destroy;
begin
  Self.fServiceRecords.Free;

  inherited Destroy;
end;

function TIdNaptrRecord.AsSipTransport: String;
begin
  // TODO: We really need to reference a transport registry of some kind.
  if      IsEqual(Self.Service, NaptrTlsService) then
    Result := TlsTransport
  else if IsEqual(Self.Service, NaptrTcpService) then
    Result := TcpTransport
  else if IsEqual(Self.Service, NaptrUdpService) then
    Result := UdpTransport
  else if IsEqual(Self.Service, NaptrSctpService) then
    Result := SctpTransport
  else if IsEqual(Self.Service, NaptrTlsOverSctpService) then
    Result := TlsOverSctpTransport
  else
    raise Exception.Create('Don''t know what transport to use for a NAPTR service ''' + Self.Service + '''');
end;

function TIdNaptrRecord.AsString: String;
begin
  // Display Self's data in BIND format.
  Result := Format('%s %s %d %d %s %s %s %s', [Self.Key, Self.ResourceType, Self.Order, Self.Preference, Self.Flags, Self.Service, Self.Regex, Self.Service]);
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

  Result.ServiceRecords.AddServiceRecords(Self.ServiceRecords);                                
end;

function TIdNaptrRecord.IsSecureService: Boolean;
var
  Service:  String;
begin
  // TODO: move to TIdNaptrRecord
  Service := Self.Service;

  Service := Fetch(Service, NaptrDelimiter);

  Result := IsEqual(NaptrSipsService, Service);
end;

function TIdNaptrRecord.ResourceType: String;
begin
  Result := DnsNAPTRRecord;
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

function TIdNaptrRecords.AnyAppropriateRecord(Transports: TStrings): TIdNaptrRecord;
var
  I: Integer;
begin
  Result := nil;
  I      := 0;

  while (I < Self.Count) and not Assigned(Result) do
    if (Transports.IndexOf(Self[I].AsSipTransport) <> ItemNotFoundIndex) then
      Result := Self[I]
    else
      Inc(I);
end;

function TIdNaptrRecords.RecordFor(ReplacementValue: String): TIdNaptrRecord;
var
  I: Integer;
begin
  // Return the first NAPTR record that has ReplacementValue as its replacement.

  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    if (Self[I].Value = ReplacementValue) then begin
      Result := Self[I];
      Break;
    end;
  end;
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

  Self.fDomain      := Domain;
  Self.fNameRecords := TIdDomainNameRecords.Create;
  Self.fPort        := Port;
  Self.fPriority    := Priority;
  Self.fService     := Service;
  Self.fTarget      := Target;
  Self.fWeight      := Weight;
end;

destructor TIdSrvRecord.Destroy;
begin
  Self.fNameRecords.Free;

  inherited Destroy;
end;

function TIdSrvRecord.AsString: String;
begin
  // Display Self's data in BIND format.
  Result := Format('%s%s %s %d %d %d %s', [Self.Service, Self.Domain, Self.ResourceType, Self.Priority, Self.Weight, Self.Port, Self.Target]);
end;

function TIdSrvRecord.Copy: TIdSrvRecord;
var
  I: Integer;
begin
  Result := TIdSrvRecord.Create(Self.Domain,
                                Self.Service,
                                Self.Priority,
                                Self.Weight,
                                Self.Port,
                                Self.Target);

  for I := 0 to Self.NameRecords.Count - 1 do
    Result.NameRecords.Add(Self.NameRecords[I]);
end;

function TIdSrvRecord.QueryName: String;
begin
  Result := Self.Service + '.' + Self.Domain;
end;

function TIdSrvRecord.ResourceType: String;
begin
  Result := DnsSRVRecord;
end;

function TIdSrvRecord.SipTransport: String;
var
  UriType:   String;
  Transport: String;
begin
  // This should actually use a registry or something?
  Transport := Self.Service;
  UriType   := Fetch(Transport, '.');

  // Strip off leading underscores
  Transport := System.Copy(Transport, 2, Length(Transport));
  UriType   := System.Copy(UriType, 2, Length(UriType));

  if IsEqual(UriType, SipsScheme) then begin
    if IsEqual(Transport, TcpTransport) then
      Result := TlsTransport
    else
      raise Exception.Create('We don''t yet know how to handle non-TCP SIPS transports (' + Self.Service + ')');
  end
  else begin
    if IsEqual(Transport, TcpTransport) then
      Result := TcpTransport
    else if IsEqual(Transport, UdpTransport) then
      Result := UdpTransport
    else if IsEqual(Transport, SctpTransport) then
      Result := SctpTransport
    else
      raise Exception.Create('We don''t know about ' + Self.Service);
  end;
end;

//******************************************************************************
//* TIdSrvRecords                                                              *
//******************************************************************************
//* TIdSrvRecords Public methods ***********************************************

class function TIdSrvRecords.TransportToPrefix(const Transport: String): String;
begin
  if IsEqual(Transport, TlsTransport) then
    Result := SrvTlsPrefix
  else if IsEqual(Transport, TcpTransport) then
    Result := SrvTcpPrefix
  else if IsEqual(Transport, UdpTransport) then
    Result := SrvUdpPrefix
  else if IsEqual(Transport, SctpTransport) then
    Result := SrvSctpPrefix
  else if IsEqual(Transport, TlsOverSctpTransport) then
    Result := SrvTlsOverSctpPrefix;
end;

function TIdSrvRecords.Add(Copy: TIdSrvRecord): TIdSrvRecord;
begin
  Result := Copy.Copy;
  Self.List.Add(Result);
end;

function TIdSrvRecords.Add(const Domain: String;
                           const Service: String;
                           Priority: Word;
                           Weight: Word;
                           Port: Cardinal;
                           const Target: String): TIdSrvRecord;
begin
  Result := TIdSrvRecord.Create(Domain,
                                Service,
                                Priority,
                                Weight,
                                Port,
                                Target);
  Self.List.Add(Result);
end;

procedure TIdSrvRecords.AddNameRecord(const RecordType: String;
                                      const Domain: String;
                                      const IPAddress: String);
var
  Srv: TIdSrvRecord;
begin
  Srv := Self.ItemFor(Domain);

  if Assigned(Srv) then
    Srv.NameRecords.Add(RecordType, Domain, IPAddress);
end;

procedure TIdSrvRecords.AddServiceRecords(SRVs: TIdSrvRecords);
var
  I: Integer;
begin
  for I := 0 to SRVs.Count - 1 do
    Self.Add(SRVs[I]);
end;

function TIdSrvRecords.Last: TIdSrvRecord;
begin
  Result := Self[Self.Count - 1];
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

function TIdSrvRecords.ItemFor(const Target: String): TIdSrvRecord;
var
  I: Integer;
begin
  I      := 0;
  Result := nil;

  while (I < Self.Count) and not Assigned(Result) do
    if IsEqual(Self[I].Target, Target) then
      Result := Self[I]
    else
      Inc(I);
end;

//******************************************************************************
//* TIdResourceRecords                                                         *
//******************************************************************************
//* TIdResourceRecords Public methods ******************************************

procedure TIdResourceRecords.AddARecord(Domain,
                                        IPAddress: String);
begin
  Self.List.Add(TIdDomainNameRecord.Create(DnsARecord, Domain, IPAddress));
end;

procedure TIdResourceRecords.AddAAAARecord(Domain,
                                           IPAddress: String);
begin
  Self.List.Add(TIdDomainNameRecord.Create(DnsAAAARecord, Domain, IPAddress));
end;

procedure TIdResourceRecords.AddCNAMERecord(CanonicalName,
                                            Alias: String);
begin
  Self.List.Add(TIdDomainNameAliasRecord.Create(CanonicalName, Alias));
end;

procedure TIdResourceRecords.AddNAPTRRecord(Key: String;
                                            Order: Word;
                                            Preference: Word;
                                            Flags,
                                            Service,
                                            Regex,
                                            Value: String);
begin
  Self.List.Add(TIdNaptrRecord.Create(Key, Order, Preference, Flags, Service, Regex, Value));
end;

procedure TIdResourceRecords.AddSRVRecord(Domain,
                                          Service: String;
                                          Priority: Word;
                                          Weight: Word;
                                          Port: Cardinal;
                                          Target: String);
begin
  Self.List.Add(TIdSrvRecord.Create(Domain, Service, Priority, Weight, Port, Target));
end;

procedure TIdResourceRecords.CollectAliases(Result: TIdDomainNameAliasRecords);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if (Self[I] is TIdDomainNameAliasRecord) then
      Result.Add(Self[I] as TIdDomainNameAliasRecord);
end;

procedure TIdResourceRecords.CollectNamePointerRecords(Result: TIdNaptrRecords);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if (Self[I] is TIdNaptrRecord) then
      Result.Add(Self[I] as TIdNaptrRecord);
end;

procedure TIdResourceRecords.CollectNameRecords(Result: TIdDomainNameRecords);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if (Self[I] is TIdDomainNameRecord) then
      Result.Add(Self[I] as TIdDomainNameRecord);
end;

procedure TIdResourceRecords.CollectServiceRecords(Result: TIdSrvRecords);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if (Self[I] is TIdSrvRecord) then
      Result.Add(Self[I] as TIdSrvRecord);
end;

function TIdResourceRecords.ContainsType(ResourceRecordType: String): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to Self.Count -1 do
    if (Self[I].ResourceType = ResourceRecordType) then begin
      Result := true;
      Break;
    end;
end;

function TIdResourceRecords.LastRecord: TIdResourceRecord;
begin
  Result := Self[Self.Count - 1];
end;

//* TIdResourceRecords Public methods ******************************************

function TIdResourceRecords.GetItems(Index: Integer): TIdResourceRecord;
begin
  Result := Self.RR[Index];
end;

end.
