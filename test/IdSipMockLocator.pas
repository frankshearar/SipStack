unit IdSipMockLocator;

interface

uses
  Classes, Contnrs, IdSipLocator, IdSipMessage;

type
  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    NameRecords:   TObjectList;
    Locations: TStrings;
    NAPTR:     TObjectList;

    function  NameRecordsAt(Index: Integer): TIdDomainNameRecord;
    function  NaptrRecAt(Index: Integer): TIdNaptrRecord;
    procedure FreeLocations;
    procedure ReorderNaptrRecs;
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

    function ResolveNameRecords(const DomainName: String): TStrings; override;
    function ResolveNAPTR(const DomainName: String): TIdNaptrRecords; override;
  end;

implementation

function NaptrSort(Item1, Item2: Pointer): Integer;
var
  A: TIdNaptrRecord;
  B: TIdNaptrRecord;
begin
  // Result < 0 if Item1 is less than Item2,
  // Result = 0 if they are equal, and
  // Result > 0 if Item1 is greater than Item2.

  A := TIdNaptrRecord(Item1);
  B := TIdNaptrRecord(Item2);

  Result := A.Order - B.Order;

  if (Result = 0) then
    Result := A.Preference - B.Preference;
end;

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.NameRecords   := TObjectList.Create(true);
  Self.Locations := TStringList.Create;
  Self.NAPTR     := TObjectList.Create(true);
end;

destructor TIdSipMockLocator.Destroy;
begin
  Self.NAPTR.Free;
  Self.FreeLocations;
  Self.Locations.Free;
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
                                 '',
                                 DomainName);
  Self.NAPTR.Add(NewNaptr);

  Self.ReorderNaptrRecs;
end;

function TIdSipMockLocator.ResolveNameRecords(const DomainName: String): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;

  for I := 0 to Self.NameRecords.Count - 1 do begin
    if (Self.NameRecordsAt(I).Domain = DomainName) then
      Result.Add(Self.NameRecordsAt(I).IPAddress);
  end;
end;

function TIdSipMockLocator.ResolveNAPTR(const DomainName: String): TIdNaptrRecords;
var
  I: Integer;
begin
  Result := TIdNaptrRecords.Create;

  for I := 0 to Self.NAPTR.Count - 1 do begin
    if (Self.NaptrRecAt(I).Key = DomainName) then
      Result.Add(Self.NaptrRecAt(I));
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

procedure TIdSipMockLocator.FreeLocations;
var
  I: Integer;
begin
  for I := 0 to Self.Locations.Count - 1 do
    Self.Locations.Objects[I].Free;
end;

procedure TIdSipMockLocator.ReorderNaptrRecs;
begin
  Self.NAPTR.Sort(NaptrSort);
end;

end.
