unit IdSipRandom;

interface

type
  TIdSipRandomNumber = class(TObject)
  public
    class function Next: Cardinal; overload; virtual;
    class function Next(Max: Cardinal): Cardinal; overload;
  end;

implementation

class function TIdSipRandomNumber.Next: Cardinal;
begin
  // TODO: Delphi's RNG is not sufficient. When we have time we shall implement
  // Ferguson/Schneier's Fortuna PRNG, as described in "Practical
  // Cryptography". If peer review shows it to be decent, that is. Or we trust
  // Schneier & Ferguson blindly.
  Result := Random(MaxInt);
end;

class function TIdSipRandomNumber.Next(Max: Cardinal): Cardinal;
begin
  repeat
    Result := Self.Next;
  until Result <= Max;

  Assert(Result <= Max, 'Result > Max');
end;

initialization
  Randomize;
end.
