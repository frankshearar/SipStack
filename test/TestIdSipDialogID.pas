unit TestIdSipDialogID;

interface

uses
  IdSipDialogID, TestFramework;

type
  TestTIdSipDialogID = class(TTestCase)
  private
    ID: TIdSipDialogID;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreationFromParameters;
    procedure TestCreationFromDialogID;
    procedure TestIsEqualToSameID;
    procedure TestIsEqualToDifferentCallID;
    procedure TestIsEqualToDifferentLocalTag;
    procedure TestIsEqualToDifferentRemoteTag;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipDialog unit tests');
  Result.AddTest(TestTIdSipDialogID.Suite);
end;

//******************************************************************************
//* TestTIdSipDialogID                                                         *
//******************************************************************************
//* TestTIdSipDialogID Public methods ******************************************

procedure TestTIdSipDialogID.SetUp;
begin
  inherited SetUp;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');
end;

procedure TestTIdSipDialogID.TearDown;
begin
  Self.ID.Free;

  inherited TearDown;
end;

//* TestTIdSipDialogID Private methods *****************************************

//* TestTIdSipDialogID Published methods ***************************************

procedure TestTIdSipDialogID.TestCreationFromParameters;
begin
  CheckEquals('1', Self.ID.CallID,    'CallID');
  CheckEquals('2', Self.ID.LocalTag,  'LocalTag');
  CheckEquals('3', Self.ID.RemoteTag, 'RemoteTag');
end;

procedure TestTIdSipDialogID.TestCreationFromDialogID;
var
  Dlg: TIdSipDialogID;
begin
  Dlg := TIdSipDialogID.Create(Self.ID);
  try
    Check(Dlg.IsEqualTo(Self.ID), 'Dialog IDs not equal');
  finally
    Dlg.Free;
  end;
end;

procedure TestTIdSipDialogID.TestIsEqualToSameID;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID);
  try
    Check(D.IsEqualTo(Self.ID), 'Same Dialog ID');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestIsEqualToDifferentCallID;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID + '1',
                             Self.ID.LocalTag,
                             Self.ID.RemoteTag);
  try
    Check(not D.IsEqualTo(Self.ID), 'Different Call-ID');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestIsEqualToDifferentLocalTag;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID,
                             Self.ID.LocalTag + '1',
                             Self.ID.RemoteTag);
  try
    Check(not D.IsEqualTo(Self.ID), 'Different Local Tag');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestIsEqualToDifferentRemoteTag;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID,
                             Self.ID.LocalTag,
                             Self.ID.RemoteTag + '1');
  try
    Check(not D.IsEqualTo(Self.ID), 'Different Remote Tag');
  finally
    D.Free;
  end;
end;

initialization
  RegisterTest('Dialog ID', Suite);
end.
