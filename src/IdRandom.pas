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
    function NextSipUserAgentBranch: String; virtual; abstract;
    function NextSipUserAgentTag: String; virtual; abstract;
  end;

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
  IdSipConsts, SysUtils;

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
  // TODO: Delphi's RNG is not sufficient. When we have time we shall implement
  // Ferguson/Schneier's Fortuna PRNG, as described in "Practical
  // Cryptography". If peer review shows it to be decent, that is. Or we trust
  // Schneier & Ferguson blindly.
  Result := Random(MaxInt);
end;

function TIdRandomNumber.NextDouble: Double;
begin
  // Return a random double in the range [0, 1].
  Result := Self.NextCardinal / High(Cardinal);
end;

function TIdRandomNumber.NextCardinal(Max: Cardinal): Cardinal;
begin
  repeat
    Result := Self.NextCardinal;
  until Result <= Max;

  Assert(Result <= Max, 'Result > Max');
end;

function TIdRandomNumber.NextHexString: String;
begin
  Result := IntToHex(Self.NextCardinal, Sizeof(Cardinal)*2);
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
  GRandomNumber.Free;
end.
