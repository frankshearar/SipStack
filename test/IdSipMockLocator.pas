unit IdSipMockLocator;

interface

uses
  Classes, Contnrs, IdSipLocator, IdSipMessage;

type
  // cf RFCs 3401-3
  TIdNaptrRec = class(TObject)
  private
    fKey:        String; // The DDDS key
    fOrder:      Word;
    fRegex:      String;
    fPreference: Word;
    fValue:      String; // The DDDS value (a domain name we can feed into SRV queries
  public
    constructor Create(const Key: String;
                       Order: Word;
                       Preference: Word;
                       const Regex: String;
                       const Value: String);

    property Key:        String read fKey;
    property Order:      Word   read fOrder;
    property Preference: Word   read fPreference;
    property Regex:      String read fRegex;
    property Value:      String read fValue;    
  end;

  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    Locations: TStrings;
    NAPTR:     TObjectList;

    function  NaptrRecAt(Index: Integer): TIdNaptrRec;
    procedure FreeLocations;
    procedure ReorderNaptrRecs;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddLocation(const AddressOfRecord: String;
                          const Transport: String;
                          const Address: String;
                          Port: Cardinal);
    procedure AddNAPTR(const AddressOfRecord: String;
                       Order: Cardinal;
                       Preference: Cardinal;
                       const DomainName: String);

    function FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; override;
    function ResolveNAPTR(const DomainName: String): TStrings; override;
  end;

implementation

function NaptrSort(Item1, Item2: Pointer): Integer;
var
  A: TIdNaptrRec;
  B: TIdNaptrRec;
begin
  // Result < 0 if Item1 is less than Item2,
  // Result = 0 if they are equal, and
  // Result > 0 if Item1 is greater than Item2.

  A := TIdNaptrRec(Item1);
  B := TIdNaptrRec(Item2);

  Result := A.Order - B.Order;

  if (Result = 0) then
    Result := A.Preference - B.Preference;
end;

//******************************************************************************
//* TIdNaptrRec                                                                *
//******************************************************************************
//* TIdNaptrRec Public methods *************************************************

constructor TIdNaptrRec.Create(const Key: String;
                                  Order: Word;
                                  Preference: Word;
                                  const Regex: String;
                                  const Value: String);
begin
  inherited Create;

  Self.fKey        := Key;
  Self.fOrder      := Order;
  Self.fPreference := Preference;
  Self.fRegex      := Regex;
  Self.fValue      := Value;
end;

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.Locations := TStringList.Create;
  Self.NAPTR     := TObjectList.Create(true);
end;

destructor TIdSipMockLocator.Destroy;
begin
  Self.NAPTR.Free;
  Self.FreeLocations;
  Self.Locations.Free;

  inherited Destroy;
end;

procedure TIdSipMockLocator.AddLocation(const AddressOfRecord: String;
                                        const Transport: String;
                                        const Address: String;
                                        Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create(Transport, Address, Port);

  Self.Locations.AddObject(AddressOfRecord, NewLocation);
end;

procedure TIdSipMockLocator.AddNAPTR(const AddressOfRecord: String;
                                     Order: Cardinal;
                                     Preference: Cardinal;
                                     const DomainName: String);
var
  NewNaptr: TIdNaptrRec;
begin
  NewNaptr := TIdNaptrRec.Create(AddressOfRecord,
                                 Order,
                                 Preference,
                                 '',
                                 DomainName);
  Self.NAPTR.Add(NewNaptr);

  Self.ReorderNaptrRecs;
end;

function TIdSipMockLocator.FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations;
var
  CurrLoc: TIdSipLocation;
  I:       Integer;
begin
  Result := TIdSipLocations.Create;

  for I := 0 to Self.Locations.Count - 1 do begin
    CurrLoc := Self.Locations.Objects[I] as TIdSipLocation;

    if (Self.Locations[I] = AddressOfRecord.AsString) then
      Result.AddLocation(CurrLoc.Transport,
                         CurrLoc.Address,
                         CurrLoc.Port);
  end;

  Self.AddUriLocation(AddressOfRecord, Result);
end;

function TIdSipMockLocator.ResolveNAPTR(const DomainName: String): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;

  for I := 0 to Self.NAPTR.Count - 1 do begin
    if (Self.NaptrRecAt(I).Key = DomainName) then
      Result.Add(Self.NaptrRecAt(I).Value);
  end;
end;

//* TIdSipMockLocator Private methods ******************************************

function TIdSipMockLocator.NaptrRecAt(Index: Integer): TIdNaptrRec;
begin
  Result := Self.NAPTR[Index] as TIdNaptrRec;
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
