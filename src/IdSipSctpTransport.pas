{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipSctpTransport;

interface

uses
  IdSipTransport;

type
  // I implement the Stream Control Transmission Protocol (RFC 3286) connections
  // for the SIP stack.
  TIdSipSCTPTransport = class(TIdSipTransport)
  public
    class function GetTransportType: String; override;
    class function SrvPrefix: String; override;
  end;

implementation

uses
  IdSipDns, IdSipMessage;

//******************************************************************************
//* TIdSipSCTPTransport                                                        *
//******************************************************************************
//* TIdSipSCTPTransport Public methods *****************************************

class function TIdSipSCTPTransport.GetTransportType: String;
begin
  Result := SctpTransport;
end;

class function TIdSipSCTPTransport.SrvPrefix: String;
begin
  Result := SrvSctpPrefix;
end;

end.
