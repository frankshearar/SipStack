unit TestIdRTPPCM;

interface

uses
  IdRTPPCM, TestFramework;

type
  TestTIdRTPPCM = class(TTestCase)
  private
    PCM: TIdRTPPCM;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPPCM unit tests');
  Result.AddTest(TestTIdRTPPCM.Suite);
end;

//******************************************************************************
//* TestTIdRTPPCM                                                              *
//******************************************************************************
//* TestTIdRTPPCM Public methods ***********************************************

procedure TestTIdRTPPCM.SetUp;
begin
  inherited SetUp;

  Self.PCM := TIdRTPPCM.Create;
end;

procedure TestTIdRTPPCM.TearDown;
begin
  Self.PCM.Free;

  inherited TearDown;
end;

//* TestTIdRTPPCM Private methods **********************************************
//* TestTIdRTPPCM Published methods ********************************************

initialization
  RegisterTest('IdRTPPCM', Suite);
end.