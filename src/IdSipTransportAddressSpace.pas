unit IdSipTransportAddressSpace;

interface

uses
  IdAddressSpace;

type
  // Use me to specify your desire to receive SIP messages on particular
  // transport type for a particular address space. For instance, you might
  // want to have a default "always use TCP" option, or might want certain
  // domains contacting you via SCTP while wanting UDP from your LAN subnet.
  TIdSipTransportSpecifier = class(TIdParameterForAddressSpace)
  private
    fTransportType: String;
  public
    property TransportType: String read fTransportType write fTransportType;
  end;

  TIdSipTransportSpecifiers = class(TIdParameterForAddressSpaceList)
  protected
    function  CreateDefaultParameter: TIdParameterForAddressSpace; override;
    function  GetDefaultTransportType: String;
    procedure SetDefaultTransportType(Value: String);
  public
    procedure RemoveTransportFor(AddressSpace: String);
    procedure SetTransportFor(AddressSpace: String; TransportType: String);
    function  TransportTypeFor(Address: String): String;

    property DefaultTransportType: String read GetDefaultTransportType write SetDefaultTransportType;
  end;

const
  NoTransport = ''; // "Don't use a transport parameter."

implementation

//******************************************************************************
//* TIdSipTransportSpecifiers                                                  *
//******************************************************************************
//* TIdSipTransportSpecifiers Public methods ***********************************

procedure TIdSipTransportSpecifiers.RemoveTransportFor(AddressSpace: String);
begin
  Self.RemoveParameter(Self.CanonicaliseAddressSpace(AddressSpace));
end;

procedure TIdSipTransportSpecifiers.SetTransportFor(AddressSpace: String; TransportType: String);
var
  Param: TIdSipTransportSpecifier;
begin
  Param := TIdSipTransportSpecifier.Create;
  Param.AddressSpace  := AddressSpace;
  Param.TransportType := TransportType;

  Self.AddParameter(Param);
end;

function TIdSipTransportSpecifiers.TransportTypeFor(Address: String): String;
var
  Param: TIdSipTransportSpecifier;
begin
  // Given an Address (typically a domain, or an IPv4 or IPv6 subnet), return
  // the transport you ask a User Agent at that address to use when contacting
  // you.

  Param := Self.FindParameterFor(Address) as TIdSipTransportSpecifier;

  if Assigned(Param) then
    Result := Param.TransportType
  else
    Result := Self.DefaultTransportType;
end;

//* TIdSipTransportSpecifiers Protected methods ********************************

function TIdSipTransportSpecifiers.CreateDefaultParameter: TIdParameterForAddressSpace;
var
  Param: TIdSipTransportSpecifier;
begin
  Param := TIdSipTransportSpecifier.Create;
  Param.TransportType := NoTransport;

  Result := Param;
end;

function TIdSipTransportSpecifiers.GetDefaultTransportType: String;
begin
  Result := (Self.DefaultParameter as TIdSipTransportSpecifier).TransportType;
end;

procedure TIdSipTransportSpecifiers.SetDefaultTransportType(Value: String);
begin
  (Self.DefaultParameter as TIdSipTransportSpecifier).TransportType := Value;
end;

end.
