unit TestIdSipLocator;

interface

uses
  IdSipLocator, TestFramework;

type
  TestTIdSipLocator = class(TTestCase)
  private
    Loc: TIdSipLocator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipLocator unit tests');
  Result.AddTest(TestTIdSipLocator.Suite);
end;

//******************************************************************************
//* TestTIdSipLocator                                                          *
//******************************************************************************
//* TestTIdSipLocator Public methods *******************************************

procedure TestTIdSipLocator.SetUp;
begin
  inherited SetUp;

  Self.Loc := TIdSipLocator.Create;
end;

procedure TestTIdSipLocator.TearDown;
begin
  Self.Loc.Free;

  inherited Destroy;
end;

initialization
  RegisterTest('SIP Location Services', Suite);
end.
