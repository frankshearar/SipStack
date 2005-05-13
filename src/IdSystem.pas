{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSystem;

interface

{ This unit contains all platform-specific (i.e., OS calls) that you might need
  to call. Currently only the Windows (2000 and more recent) platform is
  implemented.
}

function GetTickCount: Cardinal;
function GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;

implementation

uses
  Windows;

function GetTickCount: Cardinal;
var
  Count, Frequency: Int64;
  Temp:             Int64;
begin
  if QueryPerformanceFrequency(Frequency) then
    if QueryPerformanceCounter(Count) then begin
       Temp := Trunc((Count * 1000)/ Frequency);

       if (Temp > High(Cardinal)) then
         Result := Temp - High(Cardinal)
       else
         Result := Temp
    end
    else
       Result:= Windows.GetTickCount
  else
    Result:= Windows.GetTickCount;
end;

function GetTickDiff(const OldTickCount, NewTickCount : Cardinal): Cardinal;
begin
  // Remember: tick counts can roll over

  if (NewTickCount >= OldTickCount) then begin
    Result := NewTickCount - OldTickCount
  end
  else
    Result := High(Cardinal) - OldTickCount + NewTickCount;
end;

end.
