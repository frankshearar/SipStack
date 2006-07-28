{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipIndyLocator;

interface

uses
  Classes, IdDNSResolver, IdSipDns, IdSipLocator, IdSipMessage;

type
  // I try resolve names using the Indy stack. I try resolve names and regard
  // EIdDNSResolver exceptions (like No Such Name, etc) as meaning "there are
  // no records". I swallow these exceptions and return empty sets.
  TIdSipIndyLocator = class(TIdSipAbstractLocator)
  private
    Resolver: TIdDNSResolver;

    procedure CollapseChain(List: TStrings; Index: Integer; NewCanonicalName: String);
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
  IdException, IdSimpleParser, SysUtils;

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
  I:          Integer;
  A:          TARecord;
  CNAME:      TNAMERecord;
  CNAMEIndex: Integer;
  CNAMEs:     TStrings;
begin
  // We can only do A records for now.
  CNAMEs := TStringList.Create;
  try
  Self.Resolver.QueryRecords := [qtA];

  try
    Self.Resolver.Resolve(DomainName);

    // We loop over all records in the Answer, Authoritative Nameservers and
    // Additional sections. Indy doesn't differentiate between them. If we
    // find a CNAME record, we add a placeholder record, storing only the
    // DomainName. If we find an A/AAAA record we check whether we have a
    // placeholder record. If we do, we record the type & address of the record.
    // Otherwise we add a complete record to the Result.

    // Right now, we ignore CNAMEs that have no matching A records. RFC 1034
    // says that if you query for A records you'll get a pair of CNAME+A records
    // returned. It doesn't say you won't get just a CNAME. To be safe, you'd
    // really want to recurse over those CNAMEs (watching out for loops).

    // Assumptions: Say A is an alias that points to B, which points to C. We
    // assume that the A CNAME B record comes first, then B CNAME C and finally
    // the A/AAAA record/s for C.

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TNAMERecord) then begin
        CNAME := Self.Resolver.QueryResult[I] as TNAMERecord;

        CNAMEIndex := CNAMEs.IndexOfName(CNAME.Name);
        if (CNAMEIndex <> -1) then begin
          // Found a link in the CNAME chain.
          // Replace name->CNAME->CNAME2 with name->CNAME2.
          Self.CollapseChain(CNAMEs, CNAMEIndex, CNAME.HostName);
        end
        else begin
          if (CNAME.RecType = qtName) then
            CNAMEs.Add(CNAME.HostName + '=' + CNAME.Name);
        end;
      end
      else if (Self.Resolver.QueryResult[I] is TARecord) then begin
        A := Self.Resolver.QueryResult[I] as TARecord;

        if (CNAMEs.IndexOfName(A.Name) <> -1) then begin
          Result.Add(DnsARecord, CNAMEs.Values[A.Name], A.IPAddress)
        end
        else begin
          if (A.Name = DomainName) then
            Result.Add(DnsARecord, DomainName, A.IPAddress);
        end;
      end;
    end;
    except
      on EIdException do;
    end;
  finally
    CNAMEs.Free;
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

procedure TIdSipIndyLocator.CollapseChain(List: TStrings; Index: Integer; NewCanonicalName: String);
var
  Name, Alias: String;
begin
  Alias := List[Index];
  Name := Fetch(Alias, '=');
  List[Index] := NewCanonicalName + '=' + Alias;
end;

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
