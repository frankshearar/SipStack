unit IdSipRandom;

interface

type
  TIdSipRandomNumber = class(TObject)
  public
    class function Next: Cardinal; virtual;
  end;

implementation

class function TIdSipRandomNumber.Next: Cardinal;
begin
  Result := Random(MaxInt);
end;

initialization
  Randomize;
end.
