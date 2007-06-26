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
    fResolveLocallyFirst: Boolean;
    Resolver: TIdDNSResolver;

    procedure AddNaptrRecord(Result: TIdNaptrRecords;
                             NaptrRecord: TNAPTRRecord);
    procedure AddSrvRecord(Data: TQueryResult;
                           Result: TIdNaptrRecords;
                           SrvRecord: TSRVRecord); overload;
    procedure AddSrvRecord(Data: TQueryResult;
                           SRVs: TIdSrvRecords;
                           SrvRecord: TSRVRecord); overload;
    procedure CollapseChain(List: TStrings; Index: Integer; NewCanonicalName: String);
    function  GetNameServer: String;
    function  GetPort: Integer;
    function  GetTimeout: Integer;
    procedure ProcessNameRecords(Data: TQueryResult;
                                 const DomainName: String;
                                 Result: TIdDomainNameRecords);
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

    property NameServer:          String  read GetNameServer write SetNameServer;
    property Port:                Integer read GetPort write SetPort;
    property ResolveLocallyFirst: Boolean read fResolveLocallyFirst write fResolveLocallyFirst;
    property Timeout:             Integer read GetTimeout write SetTimeout;
  end;

const
  DefaultTimeout = 5000; // 5 seconds

implementation

uses
  IdException, IdSimpleParser, IdSystem, SysUtils, WinSock;

//******************************************************************************
//* TIdSipIndyLocator                                                          *
//******************************************************************************
//* TIdSipIndyLocator Public methods *******************************************

constructor TIdSipIndyLocator.Create;
begin
  inherited Create;

  Self.Resolver := TIdDNSResolver.Create(nil);

  Self.Timeout             := DefaultTimeout;
  Self.ResolveLocallyFirst := true;
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
  Addresses: TStrings;
  I:         Integer;
begin
  if Self.ResolveLocallyFirst then begin
    Addresses := TStringList.Create;
    try
      ResolveARecords(DomainName, Addresses);

      for I := 0 to Addresses.Count - 1 do
        Result.Add(DnsARecord, DomainName, Addresses[I]);
    finally
      Addresses.Free;
    end;
  end;

  if Result.IsEmpty then begin
    // We can only do A records for now.
    Self.Resolver.QueryRecords := [qtA];

    try
      Self.Resolver.Resolve(DomainName);
      Self.ProcessNameRecords(Self.Resolver.QueryResult, DomainName, Result);
    except
      on EIdException do;
    end;
  end;
end;

procedure TIdSipIndyLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                               Result: TIdNaptrRecords);
var
  I: Integer;
begin
  Self.Resolver.QueryRecords := [qtNAPTR];

  try
    Self.Resolver.Resolve(TargetUri.Host);

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TNAPTRRecord) then begin
        Self.AddNaptrRecord(Result, Self.Resolver.QueryResult[I] as TNAPTRRecord);
      end
      else if (Self.Resolver.QueryResult[I] is TSRVRecord) then begin
        Self.AddSrvRecord(Self.Resolver.QueryResult, Result, Self.Resolver.QueryResult[I] as TSRVRecord);
      end;
    end;
  except
    on EIdException do;
  end;
end;

procedure TIdSipIndyLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                             Result: TIdSrvRecords);
var
  I:   Integer;
  SRV: TSRVRecord;
begin
  Self.Resolver.QueryRecords := [qtSRV];

  try
    Self.Resolver.Resolve(ServiceAndDomain);

    for I := 0 to Self.Resolver.QueryResult.Count - 1 do begin
      if (Self.Resolver.QueryResult[I] is TSRVRecord) then begin
        SRV := Self.Resolver.QueryResult[I] as TSRVRecord;
        Self.AddSrvRecord(Self.Resolver.QueryResult, Result, SRV);
      end;
    end;
  except
    on EIdException do;
  end;
end;

//* TIdSipIndyLocator Private methods ******************************************

procedure TIdSipIndyLocator.AddNaptrRecord(Result: TIdNaptrRecords;
                                           NaptrRecord: TNAPTRRecord);
begin
  Result.Add(NaptrRecord.Name,
             NaptrRecord.Order,
             NaptrRecord.Preference,
             NaptrRecord.Flags,
             NaptrRecord.Service,
             NaptrRecord.RegExp,
             NaptrRecord.Replacement);
end;

procedure TIdSipIndyLocator.AddSrvRecord(Data: TQueryResult;
                                         Result: TIdNaptrRecords;
                                         SrvRecord: TSRVRecord);
var
  Naptr:   TIdNaptrRecord;
  Service: String;
begin
  Service := SrvRecord.Service + '.' + SrvRecord.Protocol;
  Naptr := Result.RecordFor(Service + '.' + SrvRecord.Domain);

  if Assigned(Naptr) then
    Self.AddSrvRecord(Data, Naptr.ServiceRecords, SrvRecord);
end;

procedure TIdSipIndyLocator.AddSrvRecord(Data: TQueryResult;
                                         SRVs: TIdSrvRecords;
                                         SrvRecord: TSRVRecord);
var
  NewRR: TIdSrvRecord;
begin
  NewRR := SRVs.Add(SrvRecord.Domain,
                    SrvRecord.Service + '.' + SrvRecord.Protocol,
                    SrvRecord.Priority,
                    SrvRecord.Weight,
                    SrvRecord.Port,
                    SrvRecord.Target);

  Self.ProcessNameRecords(Data, SrvRecord.Target, NewRR.NameRecords);
end;

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

procedure TIdSipIndyLocator.ProcessNameRecords(Data: TQueryResult;
                                               const DomainName: String;
                                               Result: TIdDomainNameRecords);
var
  I:          Integer;
  A:          TARecord;
  CNAME:      TNAMERecord;
  CNAMEIndex: Integer;
  CNAMEs:     TStrings;
begin
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

  CNAMEs := TStringList.Create;
  try
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
  finally
    CNAMEs.Free;
  end;
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
