unit TestIdSdpParser;

interface

uses
  IdSdpParser, TestFramework;

type
  TestTIdSdpParser = class(TTestCase)
  private
    P: TIdSdpParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdpParser unit tests');
  Result.AddTest(TestTIdSdpParser.Suite);
end;

//******************************************************************************
//* TestTIdSdpParser                                                           *
//******************************************************************************
//* TestTIdSdpParser Public methods ********************************************

procedure TestTIdSdpParser.SetUp;
begin
  inherited SetUp;

  P := TIdSdpParser.Create;
end;

procedure TestTIdSdpParser.TearDown;
begin
  P.Free;

  inherited TearDown;
end;

//* TestTIdSdpParser Private methods *******************************************
//* TestTIdSdpParser Published methods *****************************************

initialization
  RegisterTest('IdSdpParser', Suite);
end.