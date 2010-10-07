{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipLocation;

interface

uses
  Classes, IdConnectionBindings, IdSipDns;

type
  TIdSipLocation = class(TPersistent)
  private
    fTransport: String;
    fIPAddress: String;
    fPort:      TPortNum;
  protected
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor CreatePeerLocation(Connection: TIdConnectionBindings);
    constructor Create(const Transport: String;
                       const IPAddress:   String;
                             Port: TPortNum); overload;

    procedure Assign(Src: TPersistent); override;
    function  AsCompactString: String;
    function  AsString: String;
    function  Copy: TIdSipLocation;
    function  Equals(Other: TIdSipLocation): Boolean;
    function  MatchesLocal(SocketDescription: TIdConnectionBindings): Boolean;
    function  MatchesPeer(SocketDescription: TIdConnectionBindings): Boolean;
    function  IsLocalhost: Boolean;

    property Transport: String   read fTransport write fTransport;
    property IPAddress: String   read fIPAddress write fIPAddress;
    property Port:      TPortNum read fPort write fPort;
  end;

  TIdSipLocations = class(TIdBaseList)
  private
    function GetLocation(Index: Integer): TIdSipLocation;
  public
    function  AddLocation(Location: TIdSipLocation): TIdSipLocation; overload;
    function  AddLocation(const Transport: String;
                          const Address: String;
                          Port: TPortNum): TIdSipLocation; overload;
    procedure AddLocations(Locations: TIdSipLocations);
    procedure AddLocationsFromNames(const Transport: String;
                                    Port: TPortNum;
                                    Names: TIdDomainNameRecords);
    procedure AddLocationsFromSRVs(SRV: TIdSrvRecords);
    function  AddLocationToFront(Location: TIdSipLocation): TIdSipLocation; overload;
    function  AddLocationToFront(const Transport: String;
                                 const Address: String;
                                 Port: TPortNum): TIdSipLocation; overload;
    function  AsString: String;
    function  AsStringWithPrefix(Prefix: String): String;
    function  Contains(Loc: TIdSipLocation): Boolean;
    function  Copy: TIdSipLocations;
    function  First: TIdSipLocation;
    function  FirstAddressMatch(SearchLocation: TIdSipLocation): TIdSipLocation;
    function  FirstTransportMatch(SearchLocation: TIdSipLocation): TIdSipLocation;
    function  Last: TIdSipLocation;
    procedure RemoveFirst;
    procedure Remove(Location: TIdSipLocation);
    procedure RemoveAll(Locations: TIdSipLocations);

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

const
  IPv4CompactString = '%s:%d/%s';
  IPv6CompactString = '[%s]:%d/%s';
  LocationTuple = '(location transport: %s ip-address: %s port: %d)';

implementation

uses
  IdSimpleParser, SysUtils;

//******************************************************************************
//* TIdSipLocation                                                             *
//******************************************************************************
//* TIdSipLocation Public methods **********************************************

constructor TIdSipLocation.CreatePeerLocation(Connection: TIdConnectionBindings);
begin
  inherited Create;

  Self.fTransport := Connection.Transport;
  Self.fIPAddress := Connection.PeerIP;
  Self.fPort      := Connection.PeerPort;
end;

constructor TIdSipLocation.Create(const Transport: String;
                                  const IPAddress: String;
                                  Port: TPortNum);
begin
  inherited Create;

  Self.fTransport := Transport;
  Self.fIPAddress := IPAddress;
  Self.fPort      := Port;
end;

procedure TIdSipLocation.Assign(Src: TPersistent);
var
  Other: TIdSipLocation;
begin
  if (Src is TIdSipLocation) then begin
    Other := Src as TIdSipLocation;

    Self.IPAddress := Other.IPAddress;
    Self.Port      := Other.Port;
    Self.Transport := Other.Transport;
  end
  else
    inherited Assign(Src);
end;

function TIdSipLocation.AsCompactString: String;
begin
  if (TIdIPAddressParser.IPVersion(Self.IPAddress) = Id_IPv6) then
    Result := Format(IPv6CompactString, [Self.IPAddress, Self.Port, Lowercase(Self.Transport)])
  else
    Result := Format(IPv4CompactString, [Self.IPAddress, Self.Port, Lowercase(Self.Transport)]);
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

function TIdSipLocation.MatchesLocal(SocketDescription: TIdConnectionBindings): Boolean;
begin
  Result := (Self.Transport = SocketDescription.Transport)
        and (Self.IPAddress = SocketDescription.LocalIP)
        and (Self.Port      = SocketDescription.LocalPort);
end;

function TIdSipLocation.MatchesPeer(SocketDescription: TIdConnectionBindings): Boolean;
begin
  Result := (Self.Transport = SocketDescription.Transport)
        and (Self.IPAddress = SocketDescription.PeerIP)
        and (Self.Port      = SocketDescription.PeerPort);
end;

function TIdSipLocation.IsLocalhost: Boolean;
begin
  Result := Self.IPAddress = Localhost(TIdIPAddressParser.IPVersion(Self.IPAddress))
end;

//* TIdSipLocation Protected methods *******************************************

procedure TIdSipLocation.AssignTo(Dest: TPersistent);
var
  B: TIdConnectionBindings;
begin
  if (Dest is TIdConnectionBindings) then begin
    B := Dest as TIdConnectionBindings;
    B.LocalIP   := '';
    B.LocalPort := 0;
    B.PeerIP    := Self.IPAddress;
    B.PeerPort  := Self.Port;
    B.Transport := Self.Transport;
  end
  else
    inherited AssignTo(Dest);
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

function TIdSipLocations.AddLocation(Location: TIdSipLocation): TIdSipLocation;
begin
  Result := Self.AddLocation(Location.Transport, Location.IPAddress, Location.Port);
end;

function TIdSipLocations.AddLocation(const Transport: String;
                                     const Address: String;
                                     Port: TPortNum): TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Add(Result);
end;

procedure TIdSipLocations.AddLocations(Locations: TIdSipLocations);
var
  I: Integer;
begin
  for I := 0 to Locations.Count - 1 do
    Self.AddLocation(Locations[I]);
end;

procedure TIdSipLocations.AddLocationsFromNames(const Transport: String;
                                                Port: TPortNum;
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

function TIdSipLocations.AddLocationToFront(Location: TIdSipLocation): TIdSipLocation;
begin
  Result := Self.AddLocationToFront(Location.Transport, Location.IPAddress, Location.Port);
end;

function TIdSipLocations.AddLocationToFront(const Transport: String;
                                            const Address: String;
                                            Port: TPortNum): TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Insert(0, Result);
end;                                            

function TIdSipLocations.AsString: String;
begin
  Result := Self.AsStringWithPrefix('');
end;

function TIdSipLocations.AsStringWithPrefix(Prefix: String): String;
const
  Terminator = #$D#$A;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count - 1 do
    Result := Result + Prefix + Self[I].AsString + Terminator;

  Result := System.Copy(Result, 1, Length(Result) - Length(Terminator));
end;

function TIdSipLocations.Contains(Loc: TIdSipLocation): Boolean;
var
  I: Integer;
begin
 Result := false;
  for I := 0 to Self.Count - 1 do begin
    Result := Self[I].Equals(Loc);

    if Result then Break;
  end;
end;

function TIdSipLocations.Copy: TIdSipLocations;
var
  I: Integer;
begin
  Result := TIdSipLocations.Create;

  for I := 0 to Self.Count - 1 do
    Result.AddLocation(Self[I]);
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  if Self.IsEmpty then
    Result := nil
  else
    Result := Self.Items[0];
end;

function TIdSipLocations.FirstAddressMatch(SearchLocation: TIdSipLocation): TIdSipLocation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    if (Self[I].IPAddress = SearchLocation.IPAddress) then begin
      Result := Self[I];
    end;

    if    (Self[I].Transport = SearchLocation.Transport)
      and (Self[I].IPAddress = SearchLocation.IPAddress) then begin
      Result := Self[I];
      Break;
    end;
  end;
end;

function TIdSipLocations.FirstTransportMatch(SearchLocation: TIdSipLocation): TIdSipLocation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do begin
    if (Self[I].Transport = SearchLocation.Transport) then begin
      Result := Self[I];
      Break;
    end;
  end;
end;

function TIdSipLocations.Last: TIdSipLocation;
begin
  if Self.IsEmpty then
    Result := nil
  else
    Result := Self.Items[Self.Count - 1];
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

procedure TIdSipLocations.RemoveAll(Locations: TIdSipLocations);
var
  I: Integer;
begin
  for I := 0 to Locations.Count - 1 do
    Self.Remove(Locations[I]);
end;

//* TIdSipLocations Private methods ********************************************

function TIdSipLocations.GetLocation(Index: Integer): TIdSipLocation;
begin
  Result := Self.List[Index] as TidSipLocation;
end;

end.
