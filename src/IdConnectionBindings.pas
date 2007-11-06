{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdConnectionBindings;

interface

uses
  Classes;

type
  TIdConnectionBindings = class(TPersistent)
  private
    fLocalIP:   String;
    fLocalPort: Integer;
    fPeerIP:    String;
    fPeerPort:  Integer;
    fTransport: String;
  public
    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  Copy: TIdConnectionBindings;
    function  Equals(Other: TIdConnectionBindings): Boolean;

    property LocalIP:   String  read fLocalIP write fLocalIP;
    property LocalPort: Integer read fLocalPort write fLocalPort;
    property PeerIP:    String  read fPeerIP write fPeerIP;
    property PeerPort:  Integer read fPeerPort write fPeerPort;
    property Transport: String  read fTransport write fTransport;
  end;

// Miscellaneous constants
const
  BindingTuple = '(connection-bindings local-ip: %s local-port: %d peer-ip: %s peer-port: %d transport: %s)';

implementation

uses
  IdSipLocation, SysUtils;

//******************************************************************************
//* TIdConnectionBindings                                                      *
//******************************************************************************
//* TIdConnectionBindings Public methods ***************************************

procedure TIdConnectionBindings.Assign(Src: TPersistent);
var
  Loc:   TIdSipLocation;
  Other: TIdConnectionBindings;
begin
  if (Src is TIdConnectionBindings) then begin
    Other := Src as TIdConnectionBindings;

    Self.LocalIP   := Other.LocalIP;
    Self.LocalPort := Other.LocalPort;
    Self.PeerIP    := Other.PeerIP;
    Self.PeerPort  := Other.PeerPort;
    Self.Transport := Other.Transport;
  end
  else if (Src is TIdSipLocation) then begin
    Loc := Src as TIdSipLocation;
    Self.LocalIP   := '';
    Self.LocalPort := 0;
    Self.PeerIP    := Loc.IPAddress;
    Self.PeerPort  := Loc.Port;
    Self.Transport := Loc.Transport;
  end
  else
    inherited Assign(Src);
end;

function TIdConnectionBindings.AsString: String;
begin
  Result := Format(BindingTuple, [Self.LocalIP,
                                  Self.LocalPort,
                                  Self.PeerIP,
                                  Self.PeerPort,
                                  Self.Transport]);
end;

function TIdConnectionBindings.Copy: TIdConnectionBindings;
begin
  Result := TIdConnectionBindings.Create;
  Result.Assign(Self);
end;

function TIdConnectionBindings.Equals(Other: TIdConnectionBindings): Boolean;
begin
  Result := (Self.LocalIP = Other.LocalIP)
        and (Self.LocalPort = Other.LocalPort)
        and (Self.PeerIP = Other.PeerIP)
        and (Self.PeerPort = Other.PeerPort)
        and (Self.Transport = Other.Transport);
end;

end.
