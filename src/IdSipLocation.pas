unit IdSipLocation;

interface

uses
  IdSipDns;

type
  TIdSipLocation = class(TObject)
  private
    fTransport: String;
    fIPAddress: String;
    fPort:      Cardinal;
  public
    constructor Create(const Transport: String;
                       const IPAddress:   String;
                             Port: Cardinal); overload;

    function AsString: String;
    function Copy: TIdSipLocation;
    function Equals(Other: TIdSipLocation): Boolean;

    property Transport: String   read fTransport write fTransport;
    property IPAddress: String   read fIPAddress write fIPAddress;
    property Port:      Cardinal read fPort write fPort;
  end;

  TIdSipLocations = class(TIdBaseList)
  private
    function GetLocation(Index: Integer): TIdSipLocation;
  public
    procedure AddLocation(Location: TIdSipLocation); overload;
    procedure AddLocation(const Transport: String;
                          const Address: String;
                          Port: Cardinal); overload;
    procedure AddLocations(Locations: TIdSipLocations);
    procedure AddLocationsFromNames(const Transport: String;
                                    Port: Cardinal;
                                    Names: TIdDomainNameRecords);
    procedure AddLocationsFromSRVs(SRV: TIdSrvRecords);
    function  First: TIdSipLocation;
    procedure RemoveFirst;
    procedure Remove(Location: TIdSipLocation);

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

const
  LocationTuple = '(location transport: %s ip-address: %s port: %d)';

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipLocation                                                             *
//******************************************************************************
//* TIdSipLocation Public methods **********************************************

constructor TIdSipLocation.Create(const Transport: String;
                                  const IPAddress: String;
                                  Port: Cardinal);
begin
  inherited Create;

  Self.fTransport := Transport;
  Self.fIPAddress := IPAddress;
  Self.fPort      := Port;
end;

function TIdSipLocation.AsString: String;
begin
  Result := Format(LocationTuple, [Self.Transport, Self.IPAddress, Self.Port]);
end;

function TIdSipLocation.Copy: TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Self.Transport, Self.IPAddress, Self.Port);
end;

function TIdSipLocation.Equals(Other: TIdSipLocation): Boolean;
begin
  Result := (Self.Transport = Other.Transport)
        and (Self.IPAddress = Other.IPAddress)
        and (Self.Port = Other.Port);
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

procedure TIdSipLocations.AddLocation(Location: TIdSipLocation);
begin
  Self.List.Add(Location.Copy);
end;

procedure TIdSipLocations.AddLocation(const Transport: String;
                                      const Address: String;
                                      Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Add(NewLocation);
end;

procedure TIdSipLocations.AddLocations(Locations: TIdSipLocations);
var
  I: Integer;
begin
  for I := 0 to Locations.Count - 1 do
    Self.AddLocation(Locations[I]);
end;

procedure TIdSipLocations.AddLocationsFromNames(const Transport: String;
                                                Port: Cardinal;
                                                Names: TIdDomainNameRecords);
var
  I: Integer;
begin
  for I := 0 to Names.Count - 1 do
    Self.AddLocation(Transport, Names[I].IPAddress, Port);
end;

procedure TIdSipLocations.AddLocationsFromSRVs(SRV: TIdSrvRecords);
var
  I, J: Integer;
begin
  for I := 0 to Srv.Count - 1 do
    for J := 0 to Srv[I].NameRecords.Count - 1 do
      Self.AddLocation(Srv[I].SipTransport,
                       Srv[I].NameRecords[J].IPAddress,
                       Srv[I].Port);
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  Result := Self.Items[0];
end;

procedure TIdSipLocations.RemoveFirst;
begin
  Self.Delete(0);
end;

procedure TIdSipLocations.Remove(Location: TIdSipLocation);
var
  Found: Boolean;
  I:     Integer;
begin
  Found := false;
  I     := 0;
  while (I < Self.Count) and not Found do begin
    Found := Location.Equals(Self[I]);
    if not Found then Inc(I);
  end;

  if Found then Self.List.Delete(I);
end;

//* TIdSipLocations Private methods ********************************************

function TIdSipLocations.GetLocation(Index: Integer): TIdSipLocation;
begin
  Result := Self.List[Index] as TidSipLocation;
end;

end.
