unit IdSipIndyLocator;

interface

uses
  IdDNSResolver, IdSipLocator, IdSipMessage;

type
  // I try resolve names using the Indy stack. I try resolve names and regard
  // EIdDNSResolver exceptions (like No Such Name, etc) as meaning "there are
  // no records". I swallow these exceptions and return empty sets.
  TIdSipIndyLocator = class(TIdSipAbstractLocator)
  private
    Resolver: TIdDNSResolver;

    function  GetNameServer: String;
    function  GetPort: Integer;
    function  GetTimeout: Integer;
    procedure SetNameServer(const Value: String);
    procedure SetPort(Value: Integer);
    procedure SetTimeout(Value: Integer);
  protected
    procedure PerformNameLookup(const DomainName: String;
                                Result: TIdDomainNameRecords); override;
    procedure PerformNAPTRLookup(TargetUri: TIdUri;
                                 Result: TIdNaptrRecords); override;
    procedure PerformSRVLookup(const ServiceAndDomain: String;
                               Result: TIdSrvRecords); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property NameServer: String  read GetNameServer write SetNameServer;
    property Port:       Integer read GetPort write SetPort;
    property Timeout:    Integer read GetTimeout write SetTimeout;
  end;

const
  DefaultTimeout = 5000; // 5 seconds

implementation

uses
  IdException, SysUtils;

//******************************************************************************
//* TIdSipIndyLocator                                                          *
//******************************************************************************
//* TIdSipIndyLocator Public methods *******************************************

constructor TIdSipIndyLocator.Create;
begin
  inherited Create;

  Self.Resolver := TIdDNSResolver.Create(nil);

  Self.Timeout := DefaultTimeout;
end;

destructor TIdSipIndyLocator.Destroy;
begin
  Self.Resolver.Free;

  inherited Destroy;
end;

//* TIdSipIndyLocator Protected methods ****************************************

procedure TIdSipIndyLocator.PerformNameLookup(const DomainName: String;
                                              Result: TIdDomainNameRecords);
var
  I: Integer;
  A: TARecord;
begin
  // We can only do A records for now.

  Self.Resolver.QueryRecords := [qtA];

  try
    Self.Resolver.Resolve(DomainName);

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TARecord) then begin
        A := Self.Resolver.QueryResult[I] as TARecord;

        if (A.Name = DomainName) then
          Result.Add(DnsARecord, DomainName, A.IPAddress);
      end;
    end;
  except
    on EIdException do;
  end;
end;

procedure TIdSipIndyLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                               Result: TIdNaptrRecords);
var
  I:     Integer;
  NAPTR: TNAPTRRecord;
begin
  Self.Resolver.QueryRecords := [qtNAPTR];

  try
    Self.Resolver.Resolve(TargetUri.Host);

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TNAPTRRecord) then begin
        NAPTR := Self.Resolver.QueryResult[I] as TNAPTRRecord;

        Result.Add(TargetUri.Host,
                   NAPTR.Order,
                   NAPTR.Preference,
                   NAPTR.Flags,
                   NAPTR.Service,
                   NAPTR.RegExp,
                   NAPTR.Replacement);
      end;
    end;
  except
    on EIdException do;
  end;
end;

procedure TIdSipIndyLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                             Result: TIdSrvRecords);
var
  A:   TARecord;
  I:   Integer;
  SRV: TSRVRecord;
begin
  Self.Resolver.QueryRecords := [qtSRV];

  try
    Self.Resolver.Resolve(ServiceAndDomain);

    // It seems stupid to iterate over the result set twice. However, we can only
    // add A/AAAA records to SRV records that exist. Thus, the first iteration
    // collects all the SRV records, and the second adds A/AAAA records to the SRV
    // records.

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TSRVRecord) then begin
        SRV := Self.Resolver.QueryResult[I] as TSRVRecord;

        Result.Add(SRV.Domain,
                   SRV.Service + '.' + SRV.Protocol,
                   SRV.Priority,
                   SRV.Weight,
                   SRV.Port,
                   SRV.Target);
      end;
    end;

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      // NOTA BENE: We need support for AAAA records!

      if (Self.Resolver.QueryResult[I] is TARecord) then begin
        A := Self.Resolver.QueryResult[I] as TARecord;
        Result.AddNameRecord(DnsARecord, A.Name, A.IPAddress);
      end;
    end;
  except
    on EIdException do;
  end;
end;

//* TIdSipIndyLocator Private methods ******************************************

function TIdSipIndyLocator.GetNameServer: String;
begin
  Result := Self.Resolver.Host;
end;

function TIdSipIndyLocator.GetPort: Integer;
begin
  Result := Self.Resolver.Port;
end;

function TIdSipIndyLocator.GetTimeout: Integer;
begin
  Result := Self.Resolver.ReceiveTimeout;
end;

procedure TIdSipIndyLocator.SetNameServer(const Value: String);
begin
  Self.Resolver.Host := Value;
end;

procedure TIdSipIndyLocator.SetPort(Value: Integer);
begin
  Self.Resolver.Port := Value;
end;

procedure TIdSipIndyLocator.SetTimeout(Value: Integer);
begin
  Self.Resolver.ReceiveTimeout := Value;
end;

end.
