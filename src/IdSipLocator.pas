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
  IdSipMessage;

type
  TIdSipLocation = class(TObject)
  private
    fTransport: String;
    fAddress:   String;
    fPort:      Cardinal;
  public
    property Transport: String   read fTransport write fTransport;
    property Address:   String   read fAddress write fAddress;
    property Port:      Cardinal read fPort write fPort;
  end;

  TIdSipAbstractLocator = class(TObject)
  public
    function ServerFor(AddressOfRecord: TIdSipUri): TIdSipLocation; overload; virtual; abstract;
    function ServerFor(const AddressOfRecord: String): TIdSipLocation; overload;
  end;

  // I take an address-of-record SIP/SIPS URI and return a URL at which
  // a server can take a call for the given address-of-record.
  TIdSipLocator = class(TIdSipAbstractLocator)
  public
    function ServerFor(AddressOfRecord: TIdSipUri): TIdSipLocation; override;
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipAbstractLocator                                                      *
//******************************************************************************
//* TIdSipAbstractLocator Public methods ***************************************

function TIdSipAbstractLocator.ServerFor(const AddressOfRecord: String): TIdSipLocation;
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create(AddressOfRecord);
  try
    Result := Self.ServerFor(Uri);
  finally
    Uri.Free;
  end;
end;

//******************************************************************************
//* TIdSipLocator                                                              *
//******************************************************************************
//* TIdSipLocator Public methods ***********************************************

function TIdSipLocator.ServerFor(AddressOfRecord: TIdSipUri): TIdSipLocation;
begin
  raise Exception.Create('Implement TIdSipLocator.ServerFor');
end;

end.
