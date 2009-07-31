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
  IdRegisteredObject, PluggableLogging, TestFramework;

type
  TestTIdRegisteredObject = class(TTestCase)
  published
    procedure TestNewInstanceRegistersSelf;
    procedure TestInstanceUnregistersSelf;
  end;

  TestTIdObjectRegistry = class(TTestCase)
  private
    OldLogger: TLoggerProcedure;
    Reg:       TIdObjectRegistry;

    procedure CheckRegistered(O: TObject; OID: String; Msg: String);
    procedure CheckReserved(O: TObject; OID: String; Msg: String);
    procedure CheckUnregistered(O: TObject; OID: String; Msg: String);
    procedure CheckUnreserved(O: TObject; OID: String; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCollectAllObjectsOfClassClearsResultsParameter;
    procedure TestCollectAllObjectsOfClass;
    procedure TestCollectAllObjectsOfClassNoSubclasses;
    procedure TestFindObject;
    procedure TestRegisterObjectLogs;
    procedure TestRegisterObjectReservesAndUnreservesID;
    procedure TestRegisterObjectTwice;
    procedure TestReserveIDDoesntRegister;
    procedure TestReserveIDThenRegister;
    procedure TestUnreserveIDDoesntUnregister;
    procedure TestUnregisterObject;
    procedure TestUnregisterObjectLogs;
  end;

implementation

uses
  Classes, SysUtils;

var
  GLog: TStrings;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRegisteredObject unit tests');
  Result.AddTest(TestTIdRegisteredObject.Suite);
  Result.AddTest(TestTIdObjectRegistry.Suite);
end;

procedure TestLog(Description: String;
                  SourceDesc: String;
                  Severity: TSeverityLevel;
                  EventRef: Cardinal;
                  DebugInfo: String);
begin
  GLog.Add(Description);
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

  CheckNull(TIdObjectRegistry.Singleton.FindObject(RID), 'Object not unregistered');
end;

//******************************************************************************
//* TestTIdObjectRegistry                                                      *
//******************************************************************************
//* TestTIdObjectRegistry Public methods ***************************************

procedure TestTIdObjectRegistry.SetUp;
begin
  inherited SetUp;

  Self.OldLogger := PluggableLogging.Logger;
  PluggableLogging.Logger := TestLog;

  Self.Reg := TIdObjectRegistry.Create;
end;

procedure TestTIdObjectRegistry.TearDown;
begin
  Self.Reg.Free;
  PluggableLogging.Logger := Self.OldLogger;

  inherited TearDown;
end;

//* TestTIdObjectRegistry Private methods **************************************

procedure TestTIdObjectRegistry.CheckRegistered(O: TObject; OID: String; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(RegisterLogMsg, [O.ClassName, OID])), Msg);
end;

procedure TestTIdObjectRegistry.CheckReserved(O: TObject; OID: String; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(ReserveLogMsg, [O.ClassName, OID])), Msg);
end;

procedure TestTIdObjectRegistry.CheckUnregistered(O: TObject; OID: String; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(UnregisterLogMsg, [O.ClassName, OID])), Msg);
end;

procedure TestTIdObjectRegistry.CheckUnreserved(O: TObject; OID: String; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(UnreserveLogMsg, [O.ClassName, OID])), Msg);
end;

//* TestTIdObjectRegistry Published methods ************************************

procedure TestTIdObjectRegistry.TestCollectAllObjectsOfClassClearsResultsParameter;
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.Add('Arbitrary entry');

    TIdObjectRegistry.Singleton.CollectAllObjectsOfClass(TestTIdObjectRegistry, L);

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
    OID1 := TIdObjectRegistry.Singleton.RegisterObject(O1);
    O2 := TObject.Create;
    try
      OID2 := TIdObjectRegistry.Singleton.RegisterObject(O2);

      R1 := TIdRegisteredObject.Create;
      try
        R2 := TIdRegisteredObject.Create;
        try
          L := TStringList.Create;
          try
            TIdObjectRegistry.Singleton.CollectAllObjectsOfClass(TObject, L);
            CheckEquals(4, L.Count, 'CollectAllObjectsOfClass didn''t return all instances of TObject and TIdRegisteredObject');

            TIdObjectRegistry.Singleton.CollectAllObjectsOfClass(TIdRegisteredObject, L);
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
      TIdObjectRegistry.Singleton.UnregisterObject(OID2);
      O2.Free;
    end;
  finally
    TIdObjectRegistry.Singleton.UnregisterObject(OID1);
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
    SuperID := TIdObjectRegistry.Singleton.RegisterObject(Super);
    try
      Sub := TIdRegisteredObject.Create;
      try
        L := TStringList.Create;
        try
          TIdObjectRegistry.Singleton.CollectAllObjectsOfClass(TObject, L, false);

          CheckEquals(1, L.Count, 'Subclasses were also collected');
          CheckEquals(TObject, L.Objects[0].ClassType, 'Wrong class type in first slot');
        finally
          L.Free;
        end;
      finally
        Sub.Free;
      end;
    finally
      TIdObjectRegistry.Singleton.UnregisterObject(SuperID);
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
    CheckNotNull(TIdObjectRegistry.Singleton.FindObject(R.ID), 'Registered object not found');
    Check(R = TIdObjectRegistry.Singleton.FindObject(R.ID), 'Unexpected object found');

    CheckNull(TIdObjectRegistry.Singleton.FindObject(R.ID + 'fakeID'), 'Arbitrary ID returned something');
    CheckNotEquals('', R.ID, 'Object didn''t register itself (or didn''t receive an ID');
  finally
    R.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestRegisterObjectLogs;
var
  ID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ID := Self.Reg.RegisterObject(O);
    CheckRegistered(O, ID, 'Registration not logged');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestRegisterObjectReservesAndUnreservesID;
var
  ID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ID := Self.Reg.RegisterObject(O);
    CheckReserved(O, ID, 'Object not reserved');
    CheckUnreserved(O, ID, 'Object not unreserved');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestRegisterObjectTwice;
var
  ID:    String;
  NewID: String;
  O:     TObject;
begin
  O := TObject.Create;
  try
    ID := Self.Reg.RegisterObject(O);
    NewID := Self.Reg.RegisterObject(O);
    CheckEquals(ID, NewID, 'Reregistering object returned different reference');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestReserveIDDoesntRegister;
var
  ID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ID := Reg.ReserveID(O);
    CheckNull(Reg.FindObject(ID), 'Object registered as well as ID reserved');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestReserveIDThenRegister;
var
  ReservedID:   String;
  RegisteredID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ReservedID := Reg.ReserveID(O);
    CheckNull(Reg.FindObject(ReservedID), 'Object registered as well as ID reserved');

    RegisteredID := Reg.RegisterObject(O);
    CheckEquals(ReservedID, RegisteredID, 'Reserved ID <> registered ID');
    CheckNotNull(Reg.FindObject(ReservedID), 'Object not registered');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestUnreserveIDDoesntUnregister;
var
  ID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ID := Reg.ReserveID(O);
    Reg.RegisterObject(O);

    Reg.UnreserveID(ID);
    CheckNotNull(Reg.FindObject(ID), 'Object unregistered');
  finally
    O.Free;
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

  CheckNull(TIdObjectRegistry.Singleton.FindObject(RID), 'Object not unregistered');
end;

procedure TestTIdObjectRegistry.TestUnregisterObjectLogs;
var
  ID: String;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ID := Self.Reg.RegisterObject(O);
    Self.Reg.UnregisterObject(ID);
    CheckUnregistered(O, ID, 'Unregistration not logged');
  finally
    O.Free;
  end;
end;

initialization
  RegisterTest('Registered objects', Suite);
  GLog := TStringList.Create;
end.
