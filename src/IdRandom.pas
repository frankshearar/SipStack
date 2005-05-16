{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRandom;

interface

uses
  SyncObjs;

type
  TIdRandomNumber = class(TObject)
  public
    constructor Create; virtual;

    function NextCardinal: Cardinal; overload; virtual;
    function NextDouble: Double; virtual;
    function NextCardinal(Max: Cardinal): Cardinal; overload;
    function NextHexString: String;
    function NextHighestPowerOf2(N: Cardinal): Cardinal;
    function NextRandomBits(NumBits: Cardinal): Cardinal;
    function NextSipUserAgentBranch: String; virtual; abstract;
    function NextSipUserAgentTag: String; virtual; abstract;
    function NumBitsNeeded(N: Cardinal): Cardinal;
  end;

  // I do NOT supply you with cryptographically adequate random numbers!
  // Still, you can make use of me when you write test code and don't care
  // about the entropy of my results.
  TIdBasicRandomNumber = class(TIdRandomNumber)
  private
    BranchLock:  TCriticalSection;
    fLastBranch: Cardinal;

    procedure ResetLastBranch;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function NextSipUserAgentBranch: String; override;
    function NextSipUserAgentTag: String; override;
  end;

var
  GRandomNumber: TIdRandomNumber;

implementation

uses
  IdRTP, IdSipConsts, SysUtils;

//******************************************************************************
//* TIdRandomNumber                                                            *
//******************************************************************************
//* TIdRandomNumber Public methods *********************************************

constructor TIdRandomNumber.Create;
begin
  inherited Create;
end;

function TIdRandomNumber.NextCardinal: Cardinal;
begin
  Result := Self.NextRandomBits(Self.NumBitsNeeded(High(Cardinal)));
end;

function TIdRandomNumber.NextDouble: Double;
begin
  // Return a random double in the range 0 <= Result <= 1.
  Result := Self.NextCardinal / High(Cardinal);
end;

function TIdRandomNumber.NextCardinal(Max: Cardinal): Cardinal;
var
  NumBits: Byte;
begin
  NumBits := Self.NumBitsNeeded(Max);
  repeat
    Result := Self.NextRandomBits(NumBits);
  until Result <= Max;

  Assert(Result <= Max, 'Result > Max');
end;

function TIdRandomNumber.NextHexString: String;
begin
  // Generate a hex-string representing a random 32-bit number.
  Result := IntToHex(Self.NextCardinal, Sizeof(Cardinal)*2);
end;

function TIdRandomNumber.NextHighestPowerOf2(N: Cardinal): Cardinal;
begin
  Result := 1;

  // We don't shl because the below will (correctly) raise an EIntOverflow
  // where shl wouldn't.
  while (Result < N) do
    Result := MultiplyCardinal(Result, 2);
end;

function TIdRandomNumber.NextRandomBits(NumBits: Cardinal): Cardinal;
begin
  // TODO: Delphi's RNG is not sufficient. When we have time we shall implement
  // Ferguson/Schneier's Fortuna PRNG, as described in "Practical
  // Cryptography". If peer review shows it to be decent, that is. Or we trust
  // Schneier & Ferguson blindly.

  // This function lies a bit. You can easily ask for a number with 64 bits,
  // but you'll only get a 32 bit value.
  // We have a funny typecast because Random returns an Integer less than or
  // equal to High(Cardinal). What that means is that instead of
  // 0 <= N < High(Cardinal) you actually get a number
  // -High(Cardinal) <= N < High(Cardinal). Typecasting reinterprets these bits
  // as a Cardinal, giving us a full 32-bit value & not a 31-bit value.

  if (NumBits >= 32) then
    Result := Cardinal(Random(High(Cardinal)))
  else
    Result := Random(1 shl NumBits);
end;

function TIdRandomNumber.NumBitsNeeded(N: Cardinal): Cardinal;
begin
  Result := 1;
  while (N > 1) do begin
    N := N div 2;
    Inc(Result);
  end;
end;

//******************************************************************************
//* TIdBasicRandomNumber                                                       *
//******************************************************************************
//* TIdBasicRandomNumber Public methods ****************************************

constructor TIdBasicRandomNumber.Create;
begin
  inherited Create;

  Self.BranchLock := TCriticalSection.Create;
  Self.ResetLastBranch;
end;

destructor TIdBasicRandomNumber.Destroy;
begin
  Self.BranchLock.Free;

  inherited Destroy;
end;

function TIdBasicRandomNumber.NextSipUserAgentBranch: String;
begin
  Self.BranchLock.Acquire;
  try
    // TODO
    // This is a CRAP way to generate a branch.
    // cf. RFC 3261 section 8.1.1.7
    // While this (almost) satisfies the uniqueness constraint (the branch is
    // unique for the lifetime of the instantiation of the UA), it just
    // seems sensible to generate an unguessable branch.
    Result := BranchMagicCookie + IntToStr(Self.fLastBranch);

    Inc(Self.fLastBranch);
  finally
    Self.BranchLock.Release;
  end;
end;

function TIdBasicRandomNumber.NextSipUserAgentTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := Self.NextHexString
          + Self.NextHexString;
end;

//* TIdBasicRandomNumber Private methods ***************************************

procedure TIdBasicRandomNumber.ResetLastBranch;
begin
  Self.BranchLock.Acquire;
  try
    Self.fLastBranch := 0;
  finally
    Self.BranchLock.Release;
  end;
end;

initialization
  Randomize;
  GRandomNumber := TIdBasicRandomNumber.Create;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GRandomNumber.Free;
end.
