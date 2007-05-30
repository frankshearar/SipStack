{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTlsOverSctpTransport;

interface

uses
  IdSipMessage, IdSipSctpTransport;

type
  // RFC 3436
  // This class only exists, at the moment, to ensure that we don't assume that
  // TLS over TCP is the only secure transport.
  TIdSipTlsOverSctpTransport = class(TIdSipSctpTransport)
  public
    class function DefaultPort: Cardinal; override;
    class function GetTransportType: String; override;
    class function IsSecure: Boolean; override;
    class function SrvPrefix: String; override;
  end;

implementation

uses
  IdSipDns;

class function TIdSipTlsOverSctpTransport.DefaultPort: Cardinal;
begin
  Result := DefaultSipsPort;
end;

class function TIdSipTlsOverSctpTransport.GetTransportType: String;
begin
  Result := TlsOverSctpTransport;
end;

class function TIdSipTlsOverSctpTransport.IsSecure: Boolean;
begin
  Result := true;
end;

class function TIdSipTlsOverSctpTransport.SrvPrefix: String;
begin
  Result := SrvTlsOverSctpPrefix;
end;

end.
