unit IdRandom;

interface

type
  TIdRandomNumber = class(TObject)
  public
    class function NextCardinal: Cardinal; overload; virtual;
    class function NextDouble: Double; virtual;
    class function NextCardinal(Max: Cardinal): Cardinal; overload;
  end;

implementation

class function TIdRandomNumber.NextCardinal: Cardinal;
begin
  // TODO: Delphi's RNG is not sufficient. When we have time we shall implement
  // Ferguson/Schneier's Fortuna PRNG, as described in "Practical
  // Cryptography". If peer review shows it to be decent, that is. Or we trust
  // Schneier & Ferguson blindly.
  Result := Random(MaxInt);
end;

class function TIdRandomNumber.NextDouble: Double;
begin
  // Return a random double in the range [0, 1].
  Result := Self.NextCardinal / High(Cardinal);
end;

class function TIdRandomNumber.NextCardinal(Max: Cardinal): Cardinal;
begin
  repeat
    Result := Self.NextCardinal;
  until Result <= Max;

  Assert(Result <= Max, 'Result > Max');
end;

initialization
  Randomize;
end.
