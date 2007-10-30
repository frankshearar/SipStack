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
  IdSocketHandle;

function  BindingsToStr(Bindings: TIdSocketHandles): String;
procedure RaiseSocketError(Bindings: TIdSocketHandles);

implementation

uses
  IdException, SysUtils;

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

procedure RaiseSocketError(Bindings: TIdSocketHandles);
begin
  raise EIdSocketError.Create('Could not open socket on one of (' + BindingsToStr(Bindings) + ')');
end;

end.
