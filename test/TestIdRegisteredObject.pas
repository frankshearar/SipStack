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
  TCallbackClosure = class(TObjectClosure)
  private
    Callback: TObjectMethod;
  public
    constructor Create(Callback: TObjectMethod); reintroduce;

    procedure Execute(O: TObject); override;
  end;

  TestTIdRegisteredObject = class(TTestCase)
  published
    procedure TestNewInstanceRegistersSelf;
    procedure TestInstanceUnregistersSelf;
  end;

  TestTIfThenClosureWithMethods = class(TTestCase)
  private
    RanElseBranch: Boolean;
    RanIfBranch:   Boolean;

    procedure ElseBranch(O: TObject);
    procedure IfBranch(O: TObject);
    function  ReturnFalse(O: TObject): Boolean;
    function  ReturnTrue(O: TObject): Boolean;
  public
    procedure SetUp; override;
  published
    procedure TestCopy;
    procedure TestElseBranch;
    procedure TestIfBranch;
  end;

  TestTIfExistsClosure = class(TTestCase)
  private
    C:                    TIfExistsClosure;
    IfExistsClosure:      TCallbackClosure;
    IfNotExistsClosure:   TCallbackClosure;
    RanIfExistsBranch:    Boolean;
    RanIfNotExistsBranch: Boolean;

    procedure IfExistsBranch(O: TObject);
    procedure IfNotExistsBranch(O: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestIfExistsBranch;
    procedure TestIfNotExistsBranch;
  end;

  TestTIfExistsClosureWithMethods = class(TTestCase)
  private
    C:                    TIfExistsClosureWithMethods;
    RanIfExistsBranch:    Boolean;
    RanIfNotExistsBranch: Boolean;

    procedure IfExistsBranch(O: TObject);
    procedure IfNotExistsBranch(O: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestIfExistsBranch;
    procedure TestIfNotExistsBranch;
  end;

  TestTIdObjectRegistry = class(TTestCase)
  private
    OldLogger: TLoggerProcedure;
    Reg:       TIdObjectRegistry;

    procedure CheckRegistered(O: TObject; OID: TRegisteredObjectID; Msg: String);
    procedure CheckReserved(O: TObject; OID: TRegisteredObjectID; Msg: String);
    procedure CheckUnregistered(O: TObject; OID: TRegisteredObjectID; Msg: String);
    procedure CheckUnreserved(O: TObject; OID: TRegisteredObjectID; Msg: String);
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
  Result.AddTest(TestTIfThenClosureWithMethods.Suite);
  Result.AddTest(TestTIfExistsClosure.Suite);
  Result.AddTest(TestTIfExistsClosureWithMethods.Suite);
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
//* TCallbackClosure                                                           *
//******************************************************************************
//* TCallbackClosure Public methods ********************************************

constructor TCallbackClosure.Create(Callback: TObjectMethod);
begin
  inherited Create;

  Self.Callback := Callback;
end;

procedure TCallbackClosure.Execute(O: TObject);
begin
  Self.Callback(Self);
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
    CheckNotEquals('', OidAsString(R.ID), 'Object didn''t register itself (or didn''t receive an ID');
  finally
    R.Free;
  end;
end;

procedure TestTIdRegisteredObject.TestInstanceUnregistersSelf;
var
  R:   TIdRegisteredObject;
  RID: TRegisteredObjectID;
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
//* TestTIfThenClosureWithMethods                                              *
//******************************************************************************
//* TestTIfThenClosureWithMethods Public methods *******************************

procedure TestTIfThenClosureWithMethods.SetUp;
begin
  inherited SetUp;

  Self.RanElseBranch := false;
  Self.RanIfBranch   := false;
end;

//* TestTIfThenClosureWithMethods Private methods ******************************

procedure TestTIfThenClosureWithMethods.ElseBranch(O: TObject);
begin
  Self.RanElseBranch := true;
end;

procedure TestTIfThenClosureWithMethods.IfBranch(O: TObject);
begin
  Self.RanIfBranch := true;
end;

function TestTIfThenClosureWithMethods.ReturnFalse(O: TObject): Boolean;
begin
  Result := false;
end;

function TestTIfThenClosureWithMethods.ReturnTrue(O: TObject): Boolean;
begin
  Result := true;
end;

//* TestTIfThenClosureWithMethods Published methods ****************************

procedure TestTIfThenClosureWithMethods.TestCopy;
var
  C:    TIfThenClosureWithMethods;
  Copy: TObjectClosure;
begin
  C := TIfThenClosureWithMethods.Create(Self.ReturnFalse, Self.IfBranch, Self.ElseBranch);
  try
    Copy := C.Copy;
    try
      CheckEquals(C.ClassType, Copy.ClassType, 'Copy is wrong class');

      C.Execute(C);
      Check(Self.RanElseBranch, 'Copy didn''t copy correctly');
    finally
      Copy.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIfThenClosureWithMethods.TestElseBranch;
var
  C: TIfThenClosureWithMethods;
begin
  C := TIfThenClosureWithMethods.Create(Self.ReturnFalse, Self.IfBranch, Self.ElseBranch);
  try
    C.Execute(C);
    
    Check(not Self.RanIfBranch,   'If branch method run');
    Check(    Self.RanElseBranch, 'Else branch method not run');
  finally
    C.Free;
  end;
end;

procedure TestTIfThenClosureWithMethods.TestIfBranch;
var
  C: TIfThenClosureWithMethods;
begin
  C := TIfThenClosureWithMethods.Create(Self.ReturnTrue, Self.IfBranch, Self.ElseBranch);
  try
    C.Execute(C);

    Check(    Self.RanIfBranch,   'If branch method not run');
    Check(not Self.RanElseBranch, 'Else branch method run');
  finally
    C.Free;
  end;
end;

//******************************************************************************
//* TestTIfExistsClosureWithMethods                                            *
//******************************************************************************
//* TestTIfExistsClosureWithMethods Public methods *****************************

procedure TestTIfExistsClosureWithMethods.SetUp;
begin
  inherited SetUp;

  Self.C := TIfExistsClosureWithMethods.Create(Self.IfExistsBranch, Self.IfNotExistsBranch);

  Self.RanIfExistsBranch    := false;
  Self.RanIfNotExistsBranch := false;
end;

procedure TestTIfExistsClosureWithMethods.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIfExistsClosureWithMethods Private methods ****************************

procedure TestTIfExistsClosureWithMethods.IfExistsBranch(O: TObject);
begin
  Self.RanIfExistsBranch := true;
end;

procedure TestTIfExistsClosureWithMethods.IfNotExistsBranch(O: TObject);
begin
  Self.RanIfNotExistsBranch := true;
end;

//* TestTIfExistsClosureWithMethods Published methods **************************

procedure TestTIfExistsClosureWithMethods.TestCopy;
var
  Copy: TIfExistsClosureWithMethods;
  R:    TIdRegisteredObject;
begin
  Copy := TIfExistsClosureWithMethods.Create(Self.IfExistsBranch, Self.IfNotExistsBranch);
  try
    CheckEquals(Self.C.ClassType, Copy.ClassType, 'Copy of the wrong type');

    Copy.Execute(nil);
    Check(Self.RanIfNotExistsBranch, 'If-not-exists branch not copied');

    R := TIdRegisteredObject.Create;
    try
      Copy.Execute(R);
      Check(Self.RanIfExistsBranch, 'If-exists branch not copied');
    finally
      R.Free;
    end;
  finally
    Copy.Free;
  end;
end;

procedure TestTIfExistsClosureWithMethods.TestIfExistsBranch;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    Self.C.Execute(R);
    Check(    Self.RanIfExistsBranch,    'If-exists branch not run');
    Check(not Self.RanIfNotExistsBranch, 'If-not-exists branch run');

  finally
    R.Free;
  end;
end;

procedure TestTIfExistsClosureWithMethods.TestIfNotExistsBranch;
begin
  Self.C.Execute(nil);
  Check(not Self.RanIfExistsBranch,    'If-exists branch run');
  Check(    Self.RanIfNotExistsBranch, 'If-not-exists branch not run');
end;

//******************************************************************************
//* TestTIfExistsClosure                                                       *
//******************************************************************************
//* TestTIfExistsClosure Public methods ****************************************

procedure TestTIfExistsClosure.SetUp;
begin
  inherited SetUp;

  Self.IfExistsClosure := TCallbackClosure.Create(Self.IfExistsBranch);
  Self.IfNotExistsClosure := TCallbackClosure.Create(Self.IfNotExistsBranch);
  Self.C := TIfExistsClosure.Create(Self.IfExistsClosure, Self.IfNotExistsClosure);

  Self.RanIfExistsBranch    := false;
  Self.RanIfNotExistsBranch := false;
end;

procedure TestTIfExistsClosure.TearDown;
begin
  Self.C.Free;
  Self.IfNotExistsClosure.Free;
  Self.IfExistsClosure.Free;

  inherited TearDown;
end;

//* TestTIfExistsClosure Private methods ***************************************

procedure TestTIfExistsClosure.IfExistsBranch(O: TObject);
begin
  Self.RanIfExistsBranch := true;
end;

procedure TestTIfExistsClosure.IfNotExistsBranch(O: TObject);
begin
  Self.RanIfNotExistsBranch := true;
end;

//* TestTIfExistsClosure Published methods *************************************

procedure TestTIfExistsClosure.TestCopy;
var
  Copy: TObjectClosure;
  R:    TIdRegisteredObject;
begin
  Copy := Self.C.Copy;
  try
    R := TIdRegisteredObject.Create;
    try
      Copy.Execute(R);
      Check(Self.RanIfExistsBranch, 'If-exists branch not run');

      Copy.Execute(nil);
      Check(Self.RanIfNotExistsBranch, 'If-not-exists branch not run');
    finally
      R.Free;
    end;
  finally
    Copy.Free;
  end;
end;

procedure TestTIfExistsClosure.TestIfExistsBranch;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    Self.C.Execute(R);
    Check(Self.RanIfExistsBranch,        'If-exists branch not run');
    Check(not Self.RanIfNotExistsBranch, 'If-not-exists branch run');
  finally
    R.Free;
  end;
end;

procedure TestTIfExistsClosure.TestIfNotExistsBranch;
begin
  Self.C.Execute(nil);
  Check(not Self.RanIfExistsBranch,    'If-exists branch run');
  Check(    Self.RanIfNotExistsBranch, 'If-not-exists branch not run');
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

procedure TestTIdObjectRegistry.CheckRegistered(O: TObject; OID: TRegisteredObjectID; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(RegisterLogMsg, [O.ClassName, OidAsString(OID)])), Msg);
end;

procedure TestTIdObjectRegistry.CheckReserved(O: TObject; OID: TRegisteredObjectID; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(ReserveLogMsg, [O.ClassName, OidAsString(OID)])), Msg);
end;

procedure TestTIdObjectRegistry.CheckUnregistered(O: TObject; OID: TRegisteredObjectID; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(UnregisterLogMsg, [O.ClassName, OidAsString(OID)])), Msg);
end;

procedure TestTIdObjectRegistry.CheckUnreserved(O: TObject; OID: TRegisteredObjectID; Msg: String);
begin
  CheckNotEquals(-1, GLog.IndexOf(Format(UnreserveLogMsg, [O.ClassName, OidAsString(OID)])), Msg);
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
  OID1, OID2: TRegisteredObjectID;
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
  SuperID: TRegisteredObjectID;
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
  R:         TIdRegisteredObject;
  UnusedOID: TRegisteredObjectID;
begin
  UnusedOID := TIdObjectRegistry.Singleton.ReserveID(Self);

  R := TIdRegisteredObject.Create;
  try
    CheckNotNull(TIdObjectRegistry.Singleton.FindObject(R.ID), 'Registered object not found');
    Check(R = TIdObjectRegistry.Singleton.FindObject(R.ID), 'Unexpected object found');

    CheckNull(TIdObjectRegistry.Singleton.FindObject(UnusedOID), 'Arbitrary ID returned something');
    CheckNotEquals('', OidAsString(R.ID), 'Object didn''t register itself (or didn''t receive an ID');
  finally
    R.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestRegisterObjectLogs;
var
  ID: TRegisteredObjectID;
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
  ID: TRegisteredObjectID;
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
  ID:    TRegisteredObjectID;
  NewID: TRegisteredObjectID;
  O:     TObject;
begin
  O := TObject.Create;
  try
    ID := Self.Reg.RegisterObject(O);
    NewID := Self.Reg.RegisterObject(O);
    CheckEquals(OidAsString(ID), OidAsString(NewID), 'Reregistering object returned different reference');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestReserveIDDoesntRegister;
var
  ID: TRegisteredObjectID;
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
  ReservedID:   TRegisteredObjectID;
  RegisteredID: TRegisteredObjectID;
  O:  TObject;
begin
  O := TObject.Create;
  try
    ReservedID := Reg.ReserveID(O);
    CheckNull(Reg.FindObject(ReservedID), 'Object registered as well as ID reserved');

    RegisteredID := Reg.RegisterObject(O);
    CheckEquals(OidAsString(ReservedID), OidAsString(RegisteredID), 'Reserved ID <> registered ID');
    CheckNotNull(Reg.FindObject(ReservedID), 'Object not registered');
  finally
    O.Free;
  end;
end;

procedure TestTIdObjectRegistry.TestUnreserveIDDoesntUnregister;
var
  ID: TRegisteredObjectID;
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
  R:   TIdRegisteredObject;
  RID: TRegisteredObjectID;
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
  ID: TRegisteredObjectID;
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
