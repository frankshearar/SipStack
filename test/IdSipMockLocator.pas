unit IdSipMockLocator;

interface

uses
  Classes, Contnrs, IdSipLocator, IdSipMessage;

type
  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    NameRecords: TObjectList;
    NAPTR:       TIdNaptrRecords;
    SRV:         TIdSrvRecords;

    function  NameRecordsAt(Index: Integer): TIdDomainNameRecord;
    function  NaptrRecAt(Index: Integer): TIdNaptrRecord;
    function  SrvRecAt(Index: Integer): TIdSrvRecord;
  protected
    procedure PerformNameLookup(const DomainName: String;
                                Result: TStrings); override;
    procedure PerformNAPTRLookup(TargetUri: TIdUri;
                                 Result: TIdNaptrRecords); override;
    procedure PerformSRVLookup(const ServiceAndDomain: String;
                               Result: TIdSrvRecords); override;
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
  end;

implementation

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.NameRecords := TObjectList.Create(true);
  Self.NAPTR       := TIdNaptrRecords.Create;
  Self.SRV         := TIdSrvRecords.Create;
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
  Self.NameRecords.Add(NewA);
end;

procedure TIdSipMockLocator.AddAAAA(const AddressOfRecord: String;
                                    const IPv6Address: String);
var
  NewAAAA: TIdDomainNameRecord;
begin
  NewAAAA := TIdDomainNameRecord.Create(DnsAAAARecord,
                                        AddressOfRecord,
                                        IPv6Address);
  Self.NameRecords.Add(NewAAAA);
end;

procedure TIdSipMockLocator.AddNAPTR(const AddressOfRecord: String;
                                     Order: Cardinal;
                                     Preference: Cardinal;
                                     const Flags: String;
                                     const Service: String;
                                     const DomainName: String);
var
  NewNaptr: TIdNaptrRecord;
begin
  NewNaptr := TIdNaptrRecord.Create(AddressOfRecord,
                                    Order,
                                    Preference,
                                    Flags,
                                    Service,
                                    NaptrNullFlag,
                                    DomainName);
  Self.NAPTR.Add(NewNaptr);
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

  Self.SRV.Add(NewSrv);
end;

//* TIdSipMockLocator Protected methods ****************************************

procedure TIdSipMockLocator.PerformNameLookup(const DomainName: String;
                                              Result: TStrings);
var
  I: Integer;
begin
  Result.Clear;

  for I := 0 to Self.NameRecords.Count - 1 do begin
    if (Self.NameRecordsAt(I).Domain = DomainName) then
      Result.Add(Self.NameRecordsAt(I).IPAddress);
  end;
end;

procedure TIdSipMockLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                               Result: TIdNaptrRecords);
var
  I: Integer;
begin
  Self.NAPTR.Sort;

  Result.Clear;

  for I := 0 to Self.NAPTR.Count - 1 do begin
    if (Self.NaptrRecAt(I).Key = TargetUri.Host) then
      Result.Add(Self.NaptrRecAt(I));
  end;
end;

procedure TIdSipMockLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                             Result: TIdSrvRecords);
var
  I: Integer;
begin
  Self.SRV.Sort;

  Result.Clear;

  for I := 0 to Self.SRV.Count - 1 do begin
    if (Self.SrvRecAt(I).Service + '.' + Self.SrvRecAt(I).Domain = ServiceAndDomain) then
      Result.Add(Self.SrvRecAt(I));
  end;
end;

//* TIdSipMockLocator Private methods ******************************************

function TIdSipMockLocator.NameRecordsAt(Index: Integer): TIdDomainNameRecord;
begin
  Result := Self.NameRecords[Index] as TIdDomainNameRecord;
end;

function TIdSipMockLocator.NaptrRecAt(Index: Integer): TIdNaptrRecord;
begin
  Result := Self.NAPTR[Index] as TIdNaptrRecord;
end;

function TIdSipMockLocator.SrvRecAt(Index: Integer): TIdSrvRecord;
begin
  Result := Self.SRV[Index] as TIdSrvRecord;
end;

end.
