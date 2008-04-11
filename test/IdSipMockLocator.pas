{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockLocator;

interface

uses
  Classes, Contnrs, IdSipDns, IdSipLocator, IdSipMessage;

type
  // I represent a version of a Locator used in tests. You can use me to set up
  // arbitrary DNS environments, determining NAPTR, SRV, A, AAAA records at
  // will.
  //
  // If ReturnOnlySpecifiedRecords = false (the default value) and you look up a
  // name record (A, AAAA), I will create a random address on the fly. This
  // means you can specify the A/AAAA records you want, without forcing you to
  // do this for every hostname in your test. If you want a particular DNS setup
  // that, for instance, relies on no A/AAAA records, set
  // ReturnOnlySpecifiedRecords to true to return only those entries you specify
  // manually.
  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    fAliases:                    TIdDomainNameAliasRecords;
    fLookupCount:                Cardinal;
    fNAPTR:                      TIdNaptrRecords;
    fNameRecords:                TIdDomainNameRecords;
    fReturnOnlySpecifiedRecords: Boolean;
    fSRV:                        TIdSrvRecords;

    function AddRandomNameRecord(const DomainName: String): TIdDomainNameRecord;
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
    procedure AddCNAME(const CanonicalName: String;
                       const Alias: String);
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
    procedure Clear;
    procedure RemoveNameRecords(const DomainName: String);
    procedure ResetLookupCount;

    property Aliases:                    TIdDomainNameAliasRecords read fAliases;
    property LookupCount:                Cardinal                  read fLookupCount;
    property NameRecords:                TIdDomainNameRecords      read fNameRecords;
    property NAPTR:                      TIdNaptrRecords           read fNAPTR;
    property SRV:                        TIdSrvRecords             read fSRV;
    property ReturnOnlySpecifiedRecords: Boolean                   read fReturnOnlySpecifiedRecords write fReturnOnlySpecifiedRecords;
  end;

implementation

uses
  IdRandom, SysUtils;

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.fAliases     := TIdDomainNameAliasRecords.Create;
  Self.fLookupCount := 0;
  Self.fNameRecords := TIdDomainNameRecords.Create;
  Self.fNAPTR       := TIdNaptrRecords.Create;
  Self.fSRV         := TIdSrvRecords.Create;

  Self.ReturnOnlySpecifiedRecords := false;
end;

destructor TIdSipMockLocator.Destroy;
begin
  Self.SRV.Free;
  Self.NAPTR.Free;
  Self.NameRecords.Free;
  Self.Aliases.Free;

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

procedure TIdSipMockLocator.AddCNAME(const CanonicalName: String;
                                     const Alias: String);
var
  NewCNAME: TIdDomainNameAliasRecord;
begin
  NewCNAME := TIdDomainNameAliasRecord.Create(CanonicalName,
                                              Alias);
  try
    Self.Aliases.Add(NewCNAME);
  finally
    NewCNAME.Free;
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

procedure TIdSipMockLocator.Clear;
begin
  Self.SRV.Clear;
  Self.NameRecords.Clear;  
  Self.NAPTR.Clear;
end;

procedure TIdSipMockLocator.RemoveNameRecords(const DomainName: String);
var
  I: Integer;
begin
  I := 0;

  while (I < Self.NameRecords.Count) do begin
    if (Self.NameRecords[I].Domain = DomainName) then
      Self.NameRecords.Delete(I)
    else
      Inc(I);
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

  if Result.IsEmpty and not Self.ReturnOnlySpecifiedRecords then
    Result.Add(Self.AddRandomNameRecord(DomainName));
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

//* TIdSipMockLocator Private methods ******************************************

function TIdSipMockLocator.AddRandomNameRecord(const DomainName: String): TIdDomainNameRecord;
const
  LocalHost = $7f000001;
var
  RandomAddress: String;
  IPAddress:     Cardinal;
begin
  repeat
    IPAddress := GRandomNumber.NextCardinal;
  until (IPAddress <> LocalHost);

  RandomAddress := IntToStr(GRandomNumber.NextRandomBits(8)) + '.'
                 + IntToStr(GRandomNumber.NextRandomBits(8)) + '.'
                 + IntToStr(GRandomNumber.NextRandomBits(8)) + '.'
                 + IntToStr(GRandomNumber.NextRandomBits(8));

  Result := TIdDomainNameRecord.Create(DnsARecord,
                                       DomainName,
                                       RandomAddress);
  Self.NameRecords.Add(Result);
end;

end.
