{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdInterfacedObject;

interface

uses
  IdRegisteredObject;

type
  // I AM NOT a reference-counted object, nor are any of my subclasses.
  TIdInterfacedObject = class(TIdRegisteredObject, IInterface)
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
