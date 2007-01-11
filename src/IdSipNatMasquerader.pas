{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipNatMasquerader;

interface

uses
  IdInterfacedObject, IdSdp, IdSimpleParser, IdSipLocation, IdSipMessage,
  IdSipTransport;

type
  // NatAddress can contain an address conforming to the grammar defined for the
  // address type AddressType. If you supply an IPv6 address, I will add this
  // address as an IPv6 reference to those entities that require IPv6 references
  // rather than IPv6 addresses (like URIs).
  TIdSipNatMasquerader = class(TIdInterfacedObject,
                               IIdSipTransportSendingListener)
  private
    fAddressType: TIdIPVersion;
    fNatAddress:  String;

    function  AsReference(const IPAddress: String): String;
    function  HasSdp(Msg: TIdSipMessage): Boolean;
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation);
    procedure RewriteMessageContent(Msg: TIdSipMessage;
                                    const NewBody: String);
    procedure RewriteSipContactHeaders(Msg: TIdSipMessage);
    procedure RewriteSdp(Msg: TIdSipMessage);
    procedure RewriteSdpMediaDescriptionConnections(Payload: TIdSdpPayload);
    procedure RewriteSdpOrigin(Payload: TIdSdpPayload);
    procedure RewriteSdpSessionConnections(Payload: TIdSdpPayload);
  public
    constructor Create;

    procedure RewriteRequest(Request: TIdSipRequest);
    procedure RewriteResponse(Response: TIdSipResponse);

    property AddressType: TIdIPVersion read fAddressType write fAddressType;
    property NatAddress:  String       read fNatAddress write fNatAddress;
  end;

implementation

//******************************************************************************
//* TIdSipNatMasquerader                                                       *
//******************************************************************************
//* TIdSipNatMasquerader Public methods ****************************************

constructor TIdSipNatMasquerader.Create;
begin
  inherited Create;

  Self.AddressType := Id_IPv4;
end;

procedure TIdSipNatMasquerader.RewriteRequest(Request: TIdSipRequest);
begin
  Request.LastHop.SentBy := Self.NatAddress;

  if Self.HasSdp(Request) then
    Self.RewriteSdp(Request);

  Self.RewriteSipContactHeaders(Request);
end;

procedure TIdSipNatMasquerader.RewriteResponse(Response: TIdSipResponse);
begin
  if Self.HasSdp(Response) then
    Self.RewriteSdp(Response);
    
  Self.RewriteSipContactHeaders(Response);
end;

//* TIdSipNatMasquerader Private methods ***************************************

function TIdSipNatMasquerader.AsReference(const IPAddress: String): String;
begin
  if (Self.AddressType = Id_IPv6) then
    Result := '[' + IPAddress + ']'
  else
    Result := IPAddress;
end;

function TIdSipNatMasquerader.HasSdp(Msg: TIdSipMessage): Boolean;
begin
  Result := Msg.HasHeader(ContentTypeHeaderFull)
        and (Msg.ContentType = SdpMimeType);
end;

procedure TIdSipNatMasquerader.OnSendRequest(Request: TIdSipRequest;
                                             Sender: TIdSipTransport;
                                             Destination: TIdSipLocation);
begin
  Self.RewriteRequest(Request);
end;

procedure TIdSipNatMasquerader.OnSendResponse(Response: TIdSipResponse;
                                              Sender: TIdSipTransport;
                                              Destination: TIdSipLocation);
begin
  Self.RewriteResponse(Response);
end;

procedure TIdSipNatMasquerader.RewriteMessageContent(Msg: TIdSipMessage;
                                                     const NewBody: String);
begin
  Msg.Body := NewBody;
  Msg.ContentLength := Length(NewBody);
end;

procedure TIdSipNatMasquerader.RewriteSipContactHeaders(Msg: TIdSipMessage);
begin
  Msg.Contacts.First;
  while Msg.Contacts.HasNext do begin
    if Msg.Contacts.CurrentContact.Address.IsSipUri then
      Msg.Contacts.CurrentContact.Address.Host := Self.AsReference(Self.NatAddress);

    Msg.Contacts.Next;
  end;
end;

procedure TIdSipNatMasquerader.RewriteSdp(Msg: TIdSipMessage);
var
  ParsedBody: TIdSdpPayload;
begin
  ParsedBody := TIdSdpPayload.CreateFrom(Msg.Body);
  try
    Self.RewriteSdpOrigin(ParsedBody);
    Self.RewriteSdpSessionConnections(ParsedBody);
    Self.RewriteSdpMediaDescriptionConnections(ParsedBody);

    Self.RewriteMessageContent(Msg, ParsedBody.AsString);
  finally
    ParsedBody.Free;
  end;
end;

procedure TIdSipNatMasquerader.RewriteSdpMediaDescriptionConnections(Payload: TIdSdpPayload);
var
  I, J: Integer;
begin
  for I := 0 to Payload.MediaDescriptionCount - 1 do
    for J := 0 to Payload.MediaDescriptionAt(I).Connections.Count - 1 do begin
      Payload.MediaDescriptionAt(I).Connections[J].RoutableAddress := Self.NatAddress;
      Payload.MediaDescriptionAt(I).Connections[J].AddressType     := Self.AddressType;
    end;
end;

procedure TIdSipNatMasquerader.RewriteSdpOrigin(Payload: TIdSdpPayload);
begin
  Payload.Origin.Address     := Self.NatAddress;
  Payload.Origin.AddressType := Self.AddressType;
end;

procedure TIdSipNatMasquerader.RewriteSdpSessionConnections(Payload: TIdSdpPayload);
var
  I: Integer;
begin
  for I := 0 to Payload.ConnectionCount - 1 do begin
    Payload.ConnectionAt(I).RoutableAddress := Self.NatAddress;
    Payload.ConnectionAt(I).AddressType     := Self.AddressType
  end;
end;

end.
