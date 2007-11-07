{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdIndyUtils;

// This unit contains various Indy-interfacing utility functions.

interface

uses
  IdSocketHandle, SysUtils;

function  BindingsToStr(Bindings: TIdSocketHandles): String;
procedure RaiseSocketError(OriginalException: Exception; Bindings: TIdSocketHandles); 

implementation

uses
  IdException;

//******************************************************************************
//* Unit Public functions & procedures                                         *
//******************************************************************************

function BindingsToStr(Bindings: TIdSocketHandles): String;
const
  Separator = ', ';
var
  I: Integer;
begin
  Result := '';

  if (Bindings.Count = 0) then begin
    Result := '*:' + IntToStr(Bindings.DefaultPort);
    Exit;
  end;

  for I := 0 to Bindings.Count - 1 do begin
    Result := Result + Bindings[I].IP + ':' + IntToStr(Bindings[I].Port) + Separator;
  end;

  Result := Copy(Result, 1, Length(Result) - Length(Separator));
end;

procedure RaiseSocketError(OriginalException: Exception; Bindings: TIdSocketHandles);
const
  Msg = 'Could not open socket on one of (%s) (%s: %s)';
begin
  raise EIdSocketError.Create(Format(Msg, [BindingsToStr(Bindings), OriginalException.ClassName, OriginalException.Message]));
end;

end.
