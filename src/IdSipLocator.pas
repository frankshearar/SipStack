unit IdSipLocator;

interface

uses
  IdSipHeaders;

type
  // I take an address-of-record SIP/SIPS URI and return a URL at which
  // a server can take a call for the given address-of-record.
  TIdSipLocator = class(TObject)
  public
    function ServerFor(AddressOfRecord: TIdSipUri): String;
  end;

implementation

//******************************************************************************
//* TIdSipLocator                                                              *
//******************************************************************************
//* TIdSipLocator Public methods ***********************************************

function TIdSipLocator.ServerFor(AddressOfRecord: TIdSipUri): String;
begin
  Result := '';
end;

end.
