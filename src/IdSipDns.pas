unit IdSipDns;

interface

uses
  Classes, Contnrs;

type
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

  // I represent a name record of some sort. I might contain an A (RFC 1034) or
  // AAAA (RFC 1886) records, or some future name record (like, perhaps, A6
  // (RFC 2874).
  //
  // As you can see, I am a Value Object.
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

  // cf RFCs 2915, 3401-3
  // I represent a single NAPTR record. As you can see, I am a Value Object.
  //
  // My Value property gets its name from RFC 3401's nomenclature. in "classic
  // NAPTR" language it would be called "Replacement".
  TIdNaptrRecord = class(TObject)
  private
    fFlags:      String;
    fKey:        String; // The DDDS key
    fOrder:      Word;
    fPreference: Word;
    fRegex:      String;
    fService:    String;
    fValue:      String; // The DDDS value (a domain name we can feed into SRV queries)
  public
    constructor Create(const Key: String;
                       Order: Word;
                       Preference: Word;
                       const Flags: String;
                       const Service: String;
                       const Regex: String;
                       const Value: String);
    function AsSipTransport: String;
    function Copy: TIdNaptrRecord;
    function IsSecureService: Boolean;

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
    function  AnyAppropriateRecord(Transports: TStrings): TIdNaptrRecord;
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
  TIdSrvRecord = class(TObject)
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

    function  Copy: TIdSrvRecord;
    function  QueryName: String;
    function  SipTransport: String;

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
    procedure Add(Copy: TIdSrvRecord); overload;
    procedure Add(const Domain: String;
                  const Service: String;
                  Priority: Word;
                  Weight: Word;
                  Port: Cardinal;
                  const Target: String); overload;
    procedure AddNameRecord(const RecordType: String;
                            const Domain: String;
                            const IPAddress: String);
    function  Last: TIdSrvRecord;
    procedure Sort;

    property Items[Index: Integer]: TIdSrvRecord read GetItems; default;
  end;

const
  DnsARecord    = 'A';
  DnsAAAARecord = 'AAAA';

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

function DomainNameSort(Item1, Item2: Pointer): Integer;
function NaptrSort(Item1, Item2: Pointer): Integer;
function SrvSort(Item1, Item2: Pointer): Integer;

implementation

uses
  IdSimpleParser, IdSipMessage, SysUtils;

const
  ItemNotFoundIndex = -1;
  LessThan          = -1;
  GreaterThan       = 1;
  Equal             = 0;  

//******************************************************************************
//* Unit functions & procedures                                                *
//******************************************************************************
//* Unit Public functions & procedures *****************************************

function DomainNameSort(Item1, Item2: Pointer): Integer;
begin
  raise Exception.Create('Implement DomainNameSort');
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
  //     algorithm SHOULD be used to order the SRV RRs of the same
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

function TIdNaptrRecord.IsSecureService: Boolean;
var
  Service:  String;
begin
  // TODO: move to TIdNaptrRecord
  Service := Self.Service;

  Service := Fetch(Service, NaptrDelimiter);

  Result := IsEqual(NaptrSipsService, Service);
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

procedure TIdSrvRecords.Add(Copy: TIdSrvRecord);
begin
  Self.List.Add(Copy.Copy);
end;

procedure TIdSrvRecords.Add(const Domain: String;
                            const Service: String;
                            Priority: Word;
                            Weight: Word;
                            Port: Cardinal;
                            const Target: String);
var
  NewRec: TIdSrvRecord;
begin
  NewRec := TIdSrvRecord.Create(Domain,
                                Service,
                                Priority,
                                Weight,
                                Port,
                                Target);
  Self.List.Add(NewRec);
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

end.
