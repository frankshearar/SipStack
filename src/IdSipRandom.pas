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
  // TODO: This is CRAP. When we have time we shall implement Schneier's
  // Fortuna PRNG, as described in "Practical Cryptography".
  Result := Random(MaxInt);
end;

initialization
  Randomize;
end.
