unit IdInterfacedObject;

interface

type
  // I AM NOT a reference-counted object, nor are any of my subclasses.
  TIdInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

//******************************************************************************
//* TIdInterfacedObject                                                        *
//******************************************************************************
//* TIdInterfacedObject Public methods *****************************************

function TIdInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIdInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TIdInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

end.
