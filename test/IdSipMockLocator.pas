unit IdSipMockLocator;

interface

uses
  Classes, IdSipLocator, IdSipMessage;

type
  TIdSipMockLocator = class(TIdSipAbstractLocator)
  private
    Locations: TStrings;

    procedure FreeLocations;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; override;

    procedure AddLocation(const AddressOfRecord: String;
                          const Transport: String;
                          const Address: String;
                          Port: Cardinal);
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipMockLocator                                                          *
//******************************************************************************
//* TIdSipMockLocator Public methods *******************************************

constructor TIdSipMockLocator.Create;
begin
  inherited Create;

  Self.Locations := TStringList.Create;
end;

destructor TIdSipMockLocator.Destroy;
begin
  Self.FreeLocations;
  Self.Locations.Free;

  inherited Destroy;
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

procedure TIdSipMockLocator.AddLocation(const AddressOfRecord: String;
                                        const Transport: String;
                                        const Address: String;
                                        Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create;
  NewLocation.Address   := Address;
  NewLocation.Port      := Port;
  NewLocation.Transport := Transport;

  Self.Locations.AddObject(AddressOfRecord, NewLocation);
end;

//* TIdSipMockLocator Private methods ******************************************

procedure TIdSipMockLocator.FreeLocations;
var
  I: Integer;
begin
  for I := 0 to Self.Locations.Count - 1 do
    Self.Locations.Objects[I].Free;
end;

end.
