{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipLocator;

interface

uses
  Contnrs, IdSipMessage;

type
  TIdSipLocation = class(TObject)
  private
    fTransport: String;
    fAddress:   String;
    fPort:      Cardinal;
  public
    constructor Create(const Transport: String;
                       const Address:   String;
                             Port: Cardinal); overload;
    constructor Create(Via: TIdSipViaHeader); overload;
    
    function Copy: TIdSipLocation;

    property Transport: String   read fTransport;
    property Address:   String   read fAddress;
    property Port:      Cardinal read fPort;
  end;

  TIdSipLocations = class(TObject)
  private
    List: TObjectList;

    function GetLocation(Index: Integer): TIdSipLocation;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddLocation(const Transport: String;
                          const Address: String;
                          Port: Cardinal);
    function  Count: Integer;
    function  First: TIdSipLocation;
    function  IsEmpty: Boolean;
    procedure LoadFromStrings(const NaptrData: String);

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

  // Given a SIP or SIPS URI, I return (using FindServersFor) a set of tuples of
  // the form (transport, IP address, port) that you can use to send a SIP
  // message.
  TIdSipAbstractLocator = class(TObject)
  protected
    procedure AddUriLocation(AddressOfRecord: TIdSipUri;
                             List: TIdSipLocations);
    function  CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
  public
    constructor Create; virtual;

    function FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; overload; virtual; abstract;
    function FindServersFor(const AddressOfRecord: String): TIdSipLocations; overload;
  end;

  // I take an address-of-record SIP/SIPS URI and return a URL at which
  // a server can take a call for the given address-of-record.
  TIdSipLocator = class(TIdSipAbstractLocator)
  public
    function FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations; override;
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipLocation                                                             *
//******************************************************************************
//* TIdSipLocation Public methods **********************************************

constructor TIdSipLocation.Create(const Transport: String;
                                  const Address:   String;
                                  Port: Cardinal);
begin
  inherited Create;

  Self.fTransport := Transport;
  Self.fAddress   := Address;
  Self.fPort      := Port;
end;

constructor TIdSipLocation.Create(Via: TIdSipViaHeader);
begin
  inherited Create;

  Self.fTransport := Via.Transport;
  Self.fAddress   := Via.SentBy;
  Self.fPort      := Via.Port;
end;

function TIdSipLocation.Copy: TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Self.Transport, Self.Address, Self.Port);
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

constructor TIdSipLocations.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipLocations.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
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

function TIdSipLocations.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  Result := Self.Items[0];
end;

function TIdSipLocations.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

procedure TIdSipLocations.LoadFromStrings(const NaptrData: String);
begin
end;

//* TIdSipLocations Private methods ********************************************

function TIdSipLocations.GetLocation(Index: Integer): TIdSipLocation;
begin
  Result := Self.List[Index] as TidSipLocation;
end;

//******************************************************************************
//* TIdSipAbstractLocator                                                      *
//******************************************************************************
//* TIdSipAbstractLocator Public methods ***************************************

constructor TIdSipAbstractLocator.Create;
begin
  inherited Create;
end;

function TIdSipAbstractLocator.FindServersFor(const AddressOfRecord: String): TIdSipLocations;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create(AddressOfRecord);
  try
    Result := Self.FindServersFor(Uri);
  finally
    Uri.Free;
  end;
end;

//* TIdSipAbstractLocator Protected methods ************************************

procedure TIdSipAbstractLocator.AddUriLocation(AddressOfRecord: TIdSipUri;
                                               List: TIdSipLocations);
var
  UriLocation: TIdSipLocation;
begin
  UriLocation := Self.CreateLocationFromUri(AddressOfRecord);
  try
  List.AddLocation(UriLocation.Transport,
                   UriLocation.Address,
                   UriLocation.Port);
  finally
    UriLocation.Free;
  end;
end;

function TIdSipAbstractLocator.CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
var
  Address:   String;
  Port:      Cardinal;
  Transport: String;
begin
  if AddressOfRecord.HasParameter(TransportParam) then begin
    Transport := ParamToTransport(AddressOfRecord.Transport);
  end
  else begin
    if (AddressOfRecord.Scheme = SipsScheme) then
      Transport := TlsTransport
    else
      Transport := UdpTransport;
  end;

  Address := AddressOfRecord.Host;
  Port    := AddressOfRecord.Port;

  Result := TIdSipLocation.Create(Transport, Address, Port);
end;

//******************************************************************************
//* TIdSipLocator                                                              *
//******************************************************************************
//* TIdSipLocator Public methods ***********************************************

function TIdSipLocator.FindServersFor(AddressOfRecord: TIdSipUri): TIdSipLocations;
begin
  // Try get the NAPTR/SRV data.
  // Otherwise, suck the data from the URI.
  // What to do with a transport param? Just override the Transports of all
  // locations?

  Result := TIdSipLocations.Create;

  Self.AddUriLocation(AddressOfRecord, Result);
end;

end.
