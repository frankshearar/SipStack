unit IdSipTlsOverSctpTransport;

interface

uses
  IdSipMessage, IdSipTransport;

type
  // RFC 3436
  // This class only exists, at the moment, to ensure that we don't assume that
  // TLS over TCP is the only secure transport.
  TIdSipTlsOverSctpTransport = class(TIdSipSctpTransport)
  public
    class function IsSecure: Boolean; override;
    class function SrvPrefix: String; override;

    function GetTransportType: String; override;
  end;

implementation

uses
  IdSipDns;

class function TIdSipTlsOverSctpTransport.IsSecure: Boolean;
begin
  Result := true;
end;

class function TIdSipTlsOverSctpTransport.SrvPrefix: String;
begin
  Result := SrvTlsOverSctpPrefix;
end;

function TIdSipTlsOverSctpTransport.GetTransportType: String;
begin
  Result := TlsOverSctpTransport;
end;

end.
