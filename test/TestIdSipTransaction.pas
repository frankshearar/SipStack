unit TestIdSipTransaction;

interface

uses
  IdSipParser, IdSipTransaction, TestFramework;

type
  TestTIdSipClientTransaction = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipServerTransaction = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

const
  InitialT1 = 500; // ms

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransaction unit tests');
  Result.AddTest(TestTIdSipClientTransaction.Suite);
  Result.AddTest(TestTIdSipServerTransaction.Suite);
end;

//******************************************************************************
//* TestTIdSipClientTransaction                                                *
//******************************************************************************
//* TestTIdSipClientTransaction Public methods *********************************

procedure TestTIdSipClientTransaction.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipClientTransaction.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipClientTransaction Private methods ********************************
//* TestTIdSipClientTransaction Published methods ******************************

//******************************************************************************
//* TestTIdSipServerTransaction                                                *
//******************************************************************************
//* TestTIdSipServerTransaction Public methods *********************************

procedure TestTIdSipServerTransaction.SetUp;
begin
  inherited SetUp;
end;

procedure TestTIdSipServerTransaction.TearDown;
begin
  inherited TearDown;
end;

//* TestTIdSipServerTransaction Private methods ********************************
//* TestTIdSipServerTransaction Published methods ******************************

initialization
  RegisterTest('SIP Transaction layer', Suite);
end.
