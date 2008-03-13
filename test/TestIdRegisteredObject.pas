{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRegisteredObject;

interface

uses
  IdRegisteredObject, TestFramework;

type
  TestTIdRegisteredObject = class(TTestCase)
  published
    procedure TestNewInstanceRegistersSelf;
    procedure TestInstanceUnregistersSelf;
  end;

  TestTIdObjectRegistry = class(TTestCase)
  published
    procedure TestCollectAllObjectsOfClassClearsResultsParameter;
    procedure TestCollectAllObjectsOfClass;
    procedure TestCollectAllObjectsOfClassNoSubclasses;
    procedure TestFindObject;
    procedure TestUnregisterObject;
  end;

implementation

uses
  Classes;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRegisteredObject unit tests');
  Result.AddTest(TestTIdRegisteredObject.Suite);
  Result.AddTest(TestTIdObjectRegistry.Suite);
end;

//******************************************************************************
//* TestTIdRegisteredObject                                                    *
//******************************************************************************
//* TestTIdRegisteredObject Published methods **********************************

procedure TestTIdRegisteredObject.TestNewInstanceRegistersSelf;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    CheckNotEquals('', R.ID, 'Object didn''t register itself (or didn''t receive an ID');
  finally
    R.Free;
  end;
end;

procedure TestTIdRegisteredObject.TestInstanceUnregistersSelf;
var
  R:   TIdRegisteredObject;
  RID: String;
begin
  R := TIdRegisteredObject.Create;
  try
    RID := R.ID;
  finally
    R.Free;
  end;

  CheckNull(TIdObjectRegistry.FindObject(RID), 'Object not unregistered');
end;

//******************************************************************************
//* TestTIdObjectRegistry                                                      *
//******************************************************************************
//* TestTIdObjectRegistry Published methods ************************************

procedure TestTIdObjectRegistry.TestCollectAllObjectsOfClassClearsResultsParameter;
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Add('Arbitrary entry');

    TIdObjectRegistry.CollectAllObjectsOfClass(TestTIdObjectRegistry, L);

    CheckEquals(0, L.Count, 'Results parameter not first cleared');
  finally
    L.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestCollectAllObjectsOfClass;
var
  L:          TStrings;
  OID1, OID2: String;
  O1, O2:     TObject;
  R1, R2:     TIdRegisteredObject;
begin
  // To be pedantic, we really should but try..finally blocks around the calls
  // to TIdObjectRegistry.RegisterObject, with calls to
  // TIdObjectRegistry.UnregisterObject in the finally clauses, but the test is
  // already so swamped by object instantiation boilerplate!
  O1 := TObject.Create;
  try
    OID1 := TIdObjectRegistry.RegisterObject(O1);
    O2 := TObject.Create;
    try
      OID2 := TIdObjectRegistry.RegisterObject(O2);

      R1 := TIdRegisteredObject.Create;
      try
        R2 := TIdRegisteredObject.Create;
        try
          L := TStringList.Create;
          try
            TIdObjectRegistry.CollectAllObjectsOfClass(TObject, L);
            CheckEquals(4, L.Count, 'CollectAllObjectsOfClass didn''t return all instances of TObject and TIdRegisteredObject');

            TIdObjectRegistry.CollectAllObjectsOfClass(TIdRegisteredObject, L);
            CheckEquals(2, L.Count, 'CollectAllObjectsOfClass didn''t return only instances of TIdRegisteredObject');
            CheckEquals(TIdRegisteredObject, L.Objects[0].ClassType, 'Wrong class in first slot');
            CheckEquals(TIdRegisteredObject, L.Objects[1].ClassType, 'Wrong class in second slot');
          finally
            L.Free;
          end;
        finally
          R2.Free;
        end;
      finally
        R1.Free;
      end;
    finally
      TIdObjectRegistry.UnregisterObject(OID2);
      O2.Free;
    end;
  finally
    TIdObjectRegistry.UnregisterObject(OID1);
    O1.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestCollectAllObjectsOfClassNoSubclasses;
var
  L:       TStrings;
  Super:   TObject;
  SuperID: String;
  Sub:     TIdRegisteredObject;
begin
  Super := TObject.Create;
  try
    SuperID := TIdObjectRegistry.RegisterObject(Super);
    try
      Sub := TIdRegisteredObject.Create;
      try
        L := TStringList.Create;
        try
          TIdObjectRegistry.CollectAllObjectsOfClass(TObject, L, false);

          CheckEquals(1, L.Count, 'Subclasses were also collected');
          CheckEquals(TObject, L.Objects[0].ClassType, 'Wrong class type in first slot');
        finally
          L.Free;
        end;
      finally
        Sub.Free;
      end;
    finally
      TIdObjectRegistry.UnregisterObject(SuperID);
    end;
  finally
    Super.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestFindObject;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    CheckNotNull(TIdObjectRegistry.FindObject(R.ID), 'Registered object not found');
    Check(R = TIdObjectRegistry.FindObject(R.ID), 'Unexpected object found');

    CheckNull(TIdObjectRegistry.FindObject(R.ID + 'fakeID'), 'Arbitrary ID returned something');
    CheckNotEquals('', R.ID, 'Object didn''t register itself (or didn''t receive an ID');
  finally
    R.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestUnregisterObject;
var
  R: TIdRegisteredObject;
  RID: String;
begin
  R := TIdRegisteredObject.Create;
  try
    RID := R.ID;
  finally
    R.Free;
  end;

  CheckNull(TIdObjectRegistry.FindObject(RID), 'Object not unregistered');
end;

initialization
  RegisterTest('Registered objects', Suite);
end.
