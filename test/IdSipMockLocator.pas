unit IdSipMockLocator;

interface

uses
  Classes, Contnrs, IdSipDns, IdSipLocator, IdSipMessage;

type
  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    fLookupCount: Cardinal;
    fNAPTR:       TIdNaptrRecords;
    fNameRecords: TIdDomainNameRecords;
    fSRV:         TIdSrvRecords;
  protected
    procedure PerformNameLookup(const DomainName: String;
                                Result: TIdDomainNameRecords); override;
    procedure PerformNAPTRLookup(TargetUri: TIdUri;
                                 Result: TIdNaptrRecords); override;
    procedure PerformSRVLookup(const ServiceAndDomain: String;
                               Result: TIdSrvRecords); override;
    procedure PostFindServersFor(AddressOfRecord: TIdSipUri); overload; override;
    procedure PostFindServersFor(Response: TIdSipResponse); overload; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddA(const AddressOfRecord: String;
                   const IPv4Address: String);
    procedure AddAAAA(const AddressOfRecord: String;
                      const IPv6Address: String);
    procedure AddNAPTR(const AddressOfRecord: String;
                       Order: Cardinal;
                       Preference: Cardinal;
                       const Flags: String;
                       const Service: String;
                       const DomainName: String);
    procedure AddSRV(const Domain: String;
                     const Service: String;
                     Priority: Word;
                     Weight: Word;
                     Port: Cardinal;
                     const Target: String);
    procedure ResetLookupCount;

    property LookupCount: Cardinal             read fLookupCount;
    property NameRecords: TIdDomainNameRecords read fNameRecords;
    property NAPTR:       TIdNaptrRecords      read fNAPTR;
    property SRV:          TIdSrvRecords       read fSRV;
  end;

implementation

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.fLookupCount := 0;
  Self.fNameRecords := TIdDomainNameRecords.Create;
  Self.fNAPTR       := TIdNaptrRecords.Create;
  Self.fSRV         := TIdSrvRecords.Create;
end;

destructor TIdSipMockLocator.Destroy;
begin
  Self.SRV.Free;
  Self.NAPTR.Free;
  Self.NameRecords.Free;

  inherited Destroy;
end;

procedure TIdSipMockLocator.AddA(const AddressOfRecord: String;
                                 const IPv4Address: String);
var
  NewA: TIdDomainNameRecord;
begin
  NewA := TIdDomainNameRecord.Create(DnsARecord,
                                     AddressOfRecord,
                                     IPv4Address);
  try
    Self.NameRecords.Add(NewA);
  finally
    NewA.Free;
  end;
end;

procedure TIdSipMockLocator.AddAAAA(const AddressOfRecord: String;
                                    const IPv6Address: String);
var
  NewAAAA: TIdDomainNameRecord;
begin
  NewAAAA := TIdDomainNameRecord.Create(DnsAAAARecord,
                                        AddressOfRecord,
                                        IPv6Address);
  try
    Self.NameRecords.Add(NewAAAA);
  finally
    NewAAAA.Free;
  end;
end;

procedure TIdSipMockLocator.AddNAPTR(const AddressOfRecord: String;
                                     Order: Cardinal;
                                     Preference: Cardinal;
                                     const Flags: String;
                                     const Service: String;
                                     const DomainName: String);
begin
  Self.NAPTR.Add(AddressOfRecord,
                 Order,
                 Preference,
                 Flags,
                 Service,
                 NaptrNullFlag,
                 DomainName);
end;

procedure TIdSipMockLocator.AddSRV(const Domain: String;
                                   const Service: String;
                                   Priority: Word;
                                   Weight: Word;
                                   Port: Cardinal;
                                   const Target: String);
var
  NewSrv: TIdSrvRecord;
begin
  NewSrv := TIdSrvRecord.Create(Domain, Service, Priority, Weight, Port, Target);
  try
    Self.SRV.Add(NewSrv);
  finally
    NewSrv.Free;
  end;
end;

procedure TIdSipMockLocator.ResetLookupCount;
begin
  Self.fLookupCount := 0;
end;

//* TIdSipMockLocator Protected methods ****************************************

procedure TIdSipMockLocator.PerformNameLookup(const DomainName: String;
                                              Result: TIdDomainNameRecords);
var
  I: Integer;
begin
  for I := 0 to Self.NameRecords.Count - 1 do begin
    if (Self.NameRecords[I].Domain = DomainName) then
      Result.Add(Self.NameRecords[I]);
  end;
end;

procedure TIdSipMockLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                               Result: TIdNaptrRecords);
var
  I: Integer;
begin
  Self.NAPTR.Sort;

  for I := 0 to Self.NAPTR.Count - 1 do begin
    if (Self.NAPTR[I].Key = TargetUri.Host) then
      Result.Add(Self.NAPTR[I]);
  end;
end;

procedure TIdSipMockLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                             Result: TIdSrvRecords);
var
  I: Integer;
begin
  Self.SRV.Sort;

  for I := 0 to Self.SRV.Count - 1 do begin
    if (Self.SRV[I].QueryName = ServiceAndDomain) then begin
      Result.Add(Self.SRV[I]);
      Self.ResolveNameRecords(Result.Last.Target,
                              Result.Last.NameRecords);
    end;
  end;
end;

procedure TIdSipMockLocator.PostFindServersFor(AddressOfRecord: TIdSipUri);
begin
  Inc(Self.fLookupCount);
end;

procedure TIdSipMockLocator.PostFindServersFor(Response: TIdSipResponse);
begin
  Inc(Self.fLookupCount);
end;

end.
