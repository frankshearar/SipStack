{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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
    procedure TestAsString;
    procedure TestCreationFromParameters;
    procedure TestCreationFromDialogID;
    procedure TestEqualsSameID;
    procedure TestEqualsDifferentCallID;
    procedure TestEqualsDifferentLocalTag;
    procedure TestEqualsDifferentRemoteTag;
  end;

implementation

uses
  SysUtils;

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

//* TestTIdSipDialogID Published methods ***************************************

procedure TestTIdSipDialogID.TestAsString;
begin
  CheckEquals(Format(DialogIdStringForm,
                     [Self.ID.CallID,
                      Self.ID.LocalTag,
                      Self.ID.RemoteTag]),
              Self.ID.AsString,
              'AsString');
end;

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
    Check(Dlg.Equals(Self.ID), 'Dialog IDs not equal');
  finally
    Dlg.Free;
  end;
end;

procedure TestTIdSipDialogID.TestEqualsSameID;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID);
  try
    Check(D.Equals(Self.ID), 'Same Dialog ID');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestEqualsDifferentCallID;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID + '1',
                             Self.ID.LocalTag,
                             Self.ID.RemoteTag);
  try
    Check(not D.Equals(Self.ID), 'Different Call-ID');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestEqualsDifferentLocalTag;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID,
                             Self.ID.LocalTag + '1',
                             Self.ID.RemoteTag);
  try
    Check(not D.Equals(Self.ID), 'Different Local Tag');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialogID.TestEqualsDifferentRemoteTag;
var
  D: TIdSipDialogID;
begin
  D := TIdSipDialogID.Create(Self.ID.CallID,
                             Self.ID.LocalTag,
                             Self.ID.RemoteTag + '1');
  try
    Check(not D.Equals(Self.ID), 'Different Remote Tag');
  finally
    D.Free;
  end;
end;

initialization
  RegisterTest('Dialog ID', Suite);
end.
