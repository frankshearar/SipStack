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

    function GetTransportType: String; override;
  end;

implementation

class function TIdSipTlsOverSctpTransport.IsSecure: Boolean;
begin
  Result := true;
end;

function TIdSipTlsOverSctpTransport.GetTransportType: String;
begin
  Result := TlsOverSctpTransport;
end;

initialization
  TIdSipTransport.RegisterTransport(TlsOverSctpTransport,
                                    TIdSipTlsOverSctpTransport);
end.
