{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit ConfigUtils;

interface

procedure SetDefaultTruthStrings;
procedure SetFalseValues(NewValues: array of string);
procedure SetTrueValues(NewValues: array of string);
function  StrAsBool(S: String): Boolean;
function  StrAsBoolDef(S: String; Default: Boolean): Boolean;

implementation

uses
  SysUtils;

var
  FalseValues: array of string;
  TrueValues:  array of string;

//******************************************************************************
//* Unit Private procedures/functions                                          *
//******************************************************************************

function Contains(Values: array of string; Value: String): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := Low(Values) to High(Values) do begin
    if Lowercase(Values[I]) = Lowercase(Value) then begin
      Result := true;
      Break;
    end;
  end;
end;

//******************************************************************************
//* Unit Public procedures/functions                                           *
//******************************************************************************

procedure SetDefaultTruthStrings;
begin
  SetFalseValues(['false', 'no',  'disable', 'off', '0']);
  SetTrueValues(['true',   'yes', 'enable',  'on',  '1']);
end;

procedure SetFalseValues(NewValues: array of string);
var
  I: Integer;
begin
  SetLength(FalseValues, Length(NewValues));
  for I := Low(NewValues) to High(NewValues) do
    FalseValues[I] := NewValues[I];
end;

procedure SetTrueValues(NewValues: array of string);
var
  I: Integer;
begin
  SetLength(TrueValues, Length(NewValues));
  for I := Low(NewValues) to High(NewValues) do
    TrueValues[I] := NewValues[I];
end;

function StrAsBool(S: String): Boolean;
begin
  Result := Contains(TrueValues, Lowercase(S));
end;

function StrAsBoolDef(S: String; Default: Boolean): Boolean;
begin
  // If S is a TrueValue, return true.
  // If S is a FalseValue, return false.
  // Otherwise, return Default.

  if Contains(TrueValues, S) then
    Result := true
  else if Contains(FalseValues, S) then
    Result := false
  else
    Result := Default;
end;

initialization
  SetDefaultTruthStrings;
end.
