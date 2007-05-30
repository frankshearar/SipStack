{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRandom;

interface

uses
  IdRandom, TestFramework;

type
  TestTIdRandomNumber = class(TTestCase)
  private
    Random: TIdRandomNumber;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNextCardinal;
    procedure TestNextCardinalLimited;
    procedure TestNextHighestPowerOf2;
    procedure TestNumBitsNeeded;
    procedure TestNext128bitNumber;
  end;

implementation

uses
  IdSimpleParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRandom unit tests');
  Result.AddTest(TestTIdRandomNumber.Suite);
end;

//******************************************************************************
//* TestTIdRandomNumber                                                        *
//******************************************************************************
//* TestTIdRandomNumber Public methods *****************************************

procedure TestTIdRandomNumber.SetUp;
begin
  inherited SetUp;

  Self.Random := TIdBasicRandomNumber.Create;
end;

procedure TestTIdRandomNumber.TearDown;
begin
  Self.Random.Free;

  inherited TearDown;
end;

//* TestTIdRandomNumber Published methods **************************************

procedure TestTIdRandomNumber.TestNextCardinal;
begin
  // This test doesn't cover all the properties of NextCardinal. It just makes
  // sure nothing blows up.
  Self.Random.NextCardinal;
end;

procedure TestTIdRandomNumber.TestNextCardinalLimited;
begin
  // This test doesn't cover all the properties of NextCardinal - for instance,
  // we've no way of ensuring, in this test, that NextCardinal's result < N.
  // This test just makes sure nothing blows up.
  Check(Self.Random.NextCardinal(10) <= 10,
        'NextCardinal(10)');

  // These make sure that NextCardinal works with all values possible Cardinals.
  Self.Random.NextCardinal(0);
  Self.Random.NextCardinal(High(Cardinal));
end;

procedure TestTIdRandomNumber.TestNextHighestPowerOf2;
begin
  CheckEquals(1, Self.Random.NextHighestPowerOf2(0), '0');
  CheckEquals(1, Self.Random.NextHighestPowerOf2(1), '1');
  CheckEquals(2, Self.Random.NextHighestPowerOf2(2), '2');
  CheckEquals(4, Self.Random.NextHighestPowerOf2(3), '3');
  CheckEquals(4, Self.Random.NextHighestPowerOf2(4), '4');

  CheckEquals(8192,  Self.Random.NextHighestPowerOf2(8191), '8191');
  CheckEquals(8192,  Self.Random.NextHighestPowerOf2(8192), '8192');
  CheckEquals(16384, Self.Random.NextHighestPowerOf2(8193), '8193');

  CheckEquals(16384, Self.Random.NextHighestPowerOf2(9000), '9000');

  CheckEquals(IntToHex($80, 8),
              IntToHex(Self.Random.NextHighestPowerOf2($7F), 8),
              '$7F');

  CheckEquals(IntToHex($80000000, 8),
              IntToHex(Self.Random.NextHighestPowerOf2($7F000000), 8),
              '$7F000000');

  try
    Self.Random.NextHighestPowerOf2(High(Cardinal))
  except
    on EIntOverflow do;
  end;
end;

procedure TestTIdRandomNumber.TestNumBitsNeeded;
begin
  CheckEquals(1, Self.Random.NumBitsNeeded(0), '0');
  CheckEquals(1, Self.Random.NumBitsNeeded(1), '1');
  CheckEquals(2, Self.Random.NumBitsNeeded(2), '2');
  CheckEquals(2, Self.Random.NumBitsNeeded(3), '3');
  CheckEquals(3, Self.Random.NumBitsNeeded(4), '4');

  CheckEquals(7, Self.Random.NumBitsNeeded(65),  '65');
  CheckEquals(7, Self.Random.NumBitsNeeded(127), '127');
  CheckEquals(8, Self.Random.NumBitsNeeded(128), '128');

  CheckEquals(32, Self.Random.NumBitsNeeded(High(Cardinal)), 'High(Cardinal)');
end;

procedure TestTIdRandomNumber.TestNext128bitNumber;
var
  N: String;
begin
  // This is by no means an adequate test. All it does is check that the result
  // is syntactically valid.

  N := Self.Random.Next128bitNumber;
  CheckNotEquals('', N, 'Next128bitNumber returned the empty string');
  Check(TIdSimpleParser.IsHexNumber(N), 'Next128bitNumber isn''t in hex');
end;

initialization
  RegisterTest('Random numbers', Suite);
end.
