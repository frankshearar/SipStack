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
    constructor Create;
    destructor  Destroy; override;
    function ServerFor(AddressOfRecord: TIdSipUri): TIdSipLocation; override;

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

function TIdSipMockLocator.ServerFor(AddressOfRecord: TIdSipUri): TIdSipLocation;
var
  Index: Integer;
begin
  Index := Self.Locations.IndexOf(AddressOfRecord.Uri);

  if (Index <> -1) then
    Result := Self.Locations.Objects[Index] as TIdSipLocation
  else
    raise Exception.Create('Er, what to do when there''s no location?');
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
