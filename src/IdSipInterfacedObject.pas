unit IdSipInterfacedObject;

interface

type
  // I AM NOT a reference-counted object, nor are any of my subclasses.
  TIdSipInterfacedObject = class(TObject)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

//******************************************************************************
//* TIdSipInterfacedObject                                                     *
//******************************************************************************
//* TIdSipInterfacedObject Public methods **************************************

function TIdSipInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIdSipInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TIdSipInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

end.
