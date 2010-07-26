{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRegisteredObject;

interface

uses
  Classes, SyncObjs, SysUtils;

type
  TRegisteredObjectID = type String;

  // My subclasses and I automatically register/unregister ourselves to the
  // TIdObjectRegistry.
  TIdRegisteredObject = class(TObject)
  private
    fID: TRegisteredObjectID;
  public
    // "overload" so subclasses can also declare constructors called Create.
    constructor Create; overload; virtual;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property ID: TRegisteredObjectID read fID;
  end;

  TIdCollectBlock = function(O: TObject; C: TClass): Boolean;

  TObjectClosure = class(TObject)
  public
    constructor Create; virtual;

    function  Copy: TObjectClosure; virtual;
    procedure Execute(O: TObject); virtual;
  end;

  TObjectClosureClass = class of TObjectClosure;

  // If the parameter to Execute, O, exists, then execute the IfExists closure.
  // Otherwise, execute the IfNotExists closure. Either or both closures may be
  // set to nil.
  TIfExistsClosure = class(TObjectClosure)
  private
    IfExists:    TObjectClosure;
    IfNotExists: TObjectClosure;
  public
    constructor Create(IfExists: TObjectClosure; IfNotExists: TObjectClosure); reintroduce;

    function  Copy: TObjectClosure; override;
    procedure Execute(O: TObject); override;
  end;

  TObjectMethod = procedure(O: TObject) of object;
  TUnaryPredicateMethod = function(O: TObject): Boolean of object;

  TIfThenClosureWithMethods = class(TObjectClosure)
  private
    Condition:  TUnaryPredicateMethod;
    IfBranch:   TObjectMethod;
    ElseBranch: TObjectMethod;
  public
    constructor Create(Condition: TUnaryPredicateMethod; IfBranch, ElseBranch: TObjectMethod); reintroduce;

    function  Copy: TObjectClosure; override;
    procedure Execute(O: TObject); override;
  end;

  // As TIfExistsClosure but with method pointers.
  TIfExistsClosureWithMethods = class(TObjectClosure)
  private
    IfExists:    TObjectMethod;
    IfNotExists: TObjectMethod;
  public
    constructor Create(IfExists: TObjectMethod; IfNotExists: TObjectMethod); reintroduce;

    function  Copy: TObjectClosure; override;
    procedure Execute(O: TObject); override;
  end;

  // I provide facilities to unambiguously locate any (registered) object
  // resident in memory, and to allow these objects to register and unregister
  // from me as they are instantiated and freed.
  //
  // Note that you can register any instance of any kind of object, but if you
  // register an instance that isn't a TIdRegisteredObject then you have to
  // store the object ID yourself, and unregister the object.
  //
  // A note on reserving an ID versus registering an object: you reserve an ID
  // when you want a unique, unused identifier. You might be halfway through
  // initialising an object, and need that ID, but not want anything else to be
  // able to actually _do_ anything to the object (say, through WithObjectDo).
  //
  // If you do reserve an ID and don't register the object, then you MUST
  // unreserve the ID!
  //
  // Registering an object briefly reserves an ID if you haven't already
  // reserved one for the object.
  TIdObjectRegistry = class(TObject)
  private
    ObjectRegistry: TStrings;
    ReservedIDs:    TStrings;
    RegLock:        TCriticalSection;

    procedure Collect(SearchType: TClass; SearchBlock: TIdCollectBlock; Results: TStrings);
    function  FindID(L: TStrings; O: TObject): TRegisteredObjectID;
    procedure Lock;
    procedure Log(Description: String);
    function  IndexOf(L: TStrings; ObjectID: TRegisteredObjectID): Integer;
    function  ObjectAt(Index: Integer): TObject;
    function  ObjectCalled(L: TStrings; ObjectID: TRegisteredObjectID): TObject;
    function  RegisteredIDOf(O: TObject): TRegisteredObjectID;
    function  ReservedIDOf(O: TObject): TRegisteredObjectID;
    procedure Unlock;
    function  UnusedID: TRegisteredObjectID;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    class function Singleton: TIdObjectRegistry;

    procedure CollectAllObjectsOfClass(SearchType: TClass; Results: TStrings; AllowSubclassTypes: Boolean = true);
    function  FindObject(ObjectID: TRegisteredObjectID): TObject;
    function  RegisterObject(Instance: TObject): TRegisteredObjectID;
    function  ReserveID(Instance: TObject): TRegisteredObjectID;
    procedure UnregisterObject(ObjectID: TRegisteredObjectID);
    procedure UnreserveID(ObjectID: TRegisteredObjectID);
    procedure WithExtantObjectDo(ObjectID: TRegisteredObjectID; Closure: TObjectMethod);
    procedure WithObjectDo(ObjectID: TRegisteredObjectID; Closure: TObjectClosure);
  end;

  // Throw me any time something goes unexpectedly wrong with the object
  // registry - perhaps a given string doesn't point to a registered object, or
  // to an object of an unexpected type.
  ERegistry = class(Exception);

const
  RegisterLogMsg   = '%s with ID %s instantiated';
  ReserveLogMsg    = '%s with ID %s reserved';
  UnregisterLogMsg = '%s with ID %s freed';
  UnreserveLogMsg  = '%s with ID %s unreserved';

function OidAsString(OID: TRegisteredObjectID): String;

implementation

uses
  IdSystem, PluggableLogging;

const
  ItemNotFoundIndex = -1;

var
  GObjectRegistryInstance: TIdObjectRegistry;

//******************************************************************************
//* Unit Public procedures/functions                                           *
//******************************************************************************

function OidAsString(OID: TRegisteredObjectID): String;
begin
  Result := OID;
end;

//******************************************************************************
//* Unit local procedures/functions                                            *
//******************************************************************************

function CreateSortedList: TStringList;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupError;
  Result.Sorted     := true;
end;

function ClassCollector(O: TObject; C: TClass): Boolean;
begin
  Result := O.ClassType = C;
end;

function ClassOrSubclassCollector(O: TObject; C: TClass): Boolean;
begin
  Result := O is C;
end;

//******************************************************************************
//* TIdRegisteredObject                                                        *
//******************************************************************************
//* TIdRegisteredObject Public methods *****************************************

constructor TIdRegisteredObject.Create;
begin
  // Reserve an ID so that subclasses may do interesting things like schedule
  // TIdWaits with this ID. Merely reserve it so that no other instance can
  // actually reference Self through Self.ID.

  inherited Create;

  Self.fID := TIdObjectRegistry.Singleton.ReserveID(Self);
end;

procedure TIdRegisteredObject.AfterConstruction;
begin
  // Now that we have completely initialised Self, allow others to access Self.

  inherited AfterConstruction;

  TIdObjectRegistry.Singleton.RegisterObject(Self);
end;

procedure TIdRegisteredObject.BeforeDestruction;
begin
  TIdObjectRegistry.Singleton.UnregisterObject(Self.ID);

  inherited BeforeDestruction;
end;

//******************************************************************************
//* TObjectClosure                                                             *
//******************************************************************************
//* TObjectClosure Public methods **********************************************

constructor TObjectClosure.Create;
begin
  inherited Create;
end;

function TObjectClosure.Copy: TObjectClosure;
begin
  Result := TObjectClosureClass(Self.ClassType).Create;
end;

procedure TObjectClosure.Execute(O: TObject);
begin
  // By default do nothing.
end;

//******************************************************************************
//* TIfExistsClosure                                                           *
//******************************************************************************
//* TIfExistsClosure Public methods ********************************************

constructor TIfExistsClosure.Create(IfExists: TObjectClosure; IfNotExists: TObjectClosure);
begin
  inherited Create;

  Self.IfExists    := IfExists;
  Self.IfNotExists := IfNotExists;
end;

function TIfExistsClosure.Copy: TObjectClosure;
begin
  Result := TIfExistsClosure.Create(Self.IfExists, Self.IfNotExists);
end;

procedure TIfExistsClosure.Execute(O: TObject);
begin
  if Assigned(O) then begin
    if Assigned(Self.IfExists) then
      Self.IfExists.Execute(O);
  end
  else begin
    if Assigned(Self.IfNotExists) then
      Self.IfNotExists.Execute(O);
  end;
end;

//******************************************************************************
//* TIfThenClosureWithMethods                                                  *
//******************************************************************************
//* TIfThenClosureWithMethods Public methods ***********************************

constructor TIfThenClosureWithMethods.Create(Condition: TUnaryPredicateMethod; IfBranch, ElseBranch: TObjectMethod);
begin
  inherited Create;

  Self.Condition  := Condition;
  Self.IfBranch   := IfBranch;
  Self.ElseBranch := ElseBranch;
end;

function TIfThenClosureWithMethods.Copy: TObjectClosure;
begin
  Result := TIfThenClosureWithMethods.Create(Self.Condition, Self.IfBranch, Self.ElseBranch);
end;

procedure TIfThenClosureWithMethods.Execute(O: TObject);
begin
  if Self.Condition(O) then
    Self.IfBranch(O)
  else
    Self.ElseBranch(O);
end;

//******************************************************************************
//* TIfExistsClosureWithMethods                                                *
//******************************************************************************
//* TIfExistsClosureWithMethods Public methods *********************************

constructor TIfExistsClosureWithMethods.Create(IfExists: TObjectMethod; IfNotExists: TObjectMethod);
begin
  inherited Create;

  Self.IfExists    := IfExists;
  Self.IfNotExists := IfNotExists;
end;

function TIfExistsClosureWithMethods.Copy: TObjectClosure;
begin
  Result := TIfExistsClosureWithMethods.Create(Self.IfExists, Self.IfNotExists);
end;

procedure TIfExistsClosureWithMethods.Execute(O: TObject);
begin
  if Assigned(O) then begin
    if Assigned(Self.IfExists) then
      Self.IfExists(O);
  end
  else begin
    if Assigned(Self.IfNotExists) then
      Self.IfNotExists(O);
  end;
end;

//******************************************************************************
//* TIdObjectRegistry                                                          *
//******************************************************************************
//* TIdObjectRegistry Public methods *******************************************

constructor TIdObjectRegistry.Create;
begin
  inherited Create;

  Self.ObjectRegistry := CreateSortedList;
  Self.RegLock        := TCriticalSection.Create;
  Self.ReservedIDs    := CreateSortedList;
end;

destructor TIdObjectRegistry.Destroy;
begin
  Self.Lock;
  try
    Self.ReservedIDs.Free;
    Self.ObjectRegistry.Free;
  finally
    Self.Unlock;
  end;
  Self.RegLock.Free;

  inherited Destroy;
end;

class function TIdObjectRegistry.Singleton: TIdObjectRegistry;
begin
  Result := GObjectRegistryInstance;
end;

procedure TIdObjectRegistry.CollectAllObjectsOfClass(SearchType: TClass; Results: TStrings; AllowSubclassTypes: Boolean = true);
begin
  Results.Clear;

  Self.Lock;
  try
    if AllowSubclassTypes then
      Self.Collect(SearchType, ClassOrSubclassCollector, Results)
    else
      Self.Collect(SearchType, ClassCollector, Results);
  finally
    Self.Unlock;
  end;
end;

function TIdObjectRegistry.FindObject(ObjectID: TRegisteredObjectID): TObject;
begin
  // This, and all external objects that use it, have a time-of-use,
  // time-of-check issue. Right now, a TIdObjectRegistry must not be accessible
  // from multiple threads. It is in the SIP stack: transports are referenced
  // both by socket listeners and the stack's timer!).
  //
  // Move your code to use WithObjectDo instead.

  Self.Lock;
  try
    Result := Self.ObjectCalled(Self.ObjectRegistry, ObjectID);
  finally
    Self.Unlock;
  end;
end;

function TIdObjectRegistry.RegisterObject(Instance: TObject): TRegisteredObjectID;
begin
  Self.Lock;
  try
    // If Instance is already registered, we just return that ID.
    Result := Self.FindID(Self.ObjectRegistry, Instance);
    if (Result <> '') then begin
      Exit;
    end;

    Result := Self.ReserveID(Instance);
    try
      Self.ObjectRegistry.AddObject(Result, Instance);
    finally
      Self.UnreserveID(Result);
    end;
    Self.Log(Format(RegisterLogMsg, [Instance.ClassName, Result]));
  finally
    Self.Unlock;
  end;
end;

function TIdObjectRegistry.ReserveID(Instance: TObject): TRegisteredObjectID;
begin
  // If Instance has already been reserved, just return its associated ID.
  // If Instance has already been registered, just return its associated ID.
  // Otherwise, reserve a unique identifier for Instance to use at some later
  // stage.

  Self.Lock;
  try
    Result := Self.ReservedIDOf(Instance);
    if (Result <> '') then Exit;

    Result := Self.RegisteredIDOf(Instance);
    if (Result <> '') then Exit;

    Result := Self.UnusedID;
    Self.ReservedIDs.AddObject(Result, Instance);
    Self.Log(Format(ReserveLogMsg, [Instance.ClassName, Result]));
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.UnregisterObject(ObjectID: TRegisteredObjectID);
var
  Index: Integer;
begin
  Self.Lock;
  try
    Index := Self.IndexOf(Self.ObjectRegistry, ObjectID);
    if (Index <> ItemNotFoundIndex) then begin
      Self.Log(Format(UnregisterLogMsg, [Self.ObjectRegistry.Objects[Index].ClassName, ObjectID]));
      Self.ObjectRegistry.Delete(Index);
    end;
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.UnreserveID(ObjectID: TRegisteredObjectID);
var
  Index: Integer;
begin
  // If you've reserved an ID that you no longer need, and you haven't
  // registered the object, then you need to manually unreserve the ID.

  Self.Lock;
  try
    Index := Self.IndexOf(Self.ReservedIDs, ObjectID);
    if (Index <> ItemNotFoundIndex) then begin
      Self.Log(Format(UnreserveLogMsg, [Self.ReservedIDs.Objects[Index].ClassName, ObjectID]));
      Self.ReservedIDs.Delete(Index);
    end;
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.WithExtantObjectDo(ObjectID: TRegisteredObjectID; Closure: TObjectMethod);
var
  O: TObject;
begin
  // Atomically run Closure on the object with ObjectID.

  Self.Lock;
  try
    O := Self.FindObject(ObjectID);

    if Assigned(O) then
      Closure(O);
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.WithObjectDo(ObjectID: TRegisteredObjectID; Closure: TObjectClosure);
var
  O: TObject;
begin
  // Atomically run Closure on the object with ObjectID.

  Self.Lock;
  try
    O := Self.FindObject(ObjectID);
    Closure.Execute(O);
  finally
    Self.Unlock;
  end;
end;

//* TIdObjectRegistry Private methods ******************************************

procedure TIdObjectRegistry.Collect(SearchType: TClass; SearchBlock: TIdCollectBlock; Results: TStrings);
var
  I: Integer;
begin
  // PRECONDITION: You've acquired Self.Lock.

  for I := 0 to Self.ObjectRegistry.Count - 1 do begin
    if SearchBlock(Self.ObjectAt(I), SearchType) then
      Results.AddObject(Self.ObjectRegistry[I],
                        Self.ObjectRegistry.Objects[I]);
  end;
end;

function TIdObjectRegistry.FindID(L: TStrings; O: TObject): TRegisteredObjectID;
var
  Index: Integer;
begin
  // If L contains O then return its associated ID. Otherwise, return the empty
  // string.
  Index := L.IndexOfObject(O);
  if (Index = ItemNotFoundIndex) then
    Result := ''
  else
    Result := L[Index];
end;

procedure TIdObjectRegistry.Lock;
begin
  Self.RegLock.Acquire;
end;

procedure TIdObjectRegistry.Log(Description: String);
begin
  LogEntry(Description, '', slDebug, 0, '');
end;

function TIdObjectRegistry.IndexOf(L: TStrings; ObjectID: TRegisteredObjectID): Integer;
var
  C:          Integer;
  Middle:     Integer;
  StartIndex: Integer;
  EndIndex:   Integer;
begin
  // Find the INDEX of the object identified by ObjectID in L.

  StartIndex := 0;
  EndIndex   := L.Count - 1;
  Result := ItemNotFoundIndex;

  if (StartIndex > EndIndex) then Exit;

  while (StartIndex <= EndIndex) do begin
    Middle := (EndIndex + StartIndex) div 2;

    C := CompareStr(ObjectID, L[Middle]);

    if (C < 0) then
      EndIndex := Middle - 1
    else if (C > 0) then
      StartIndex := Middle + 1
    else begin
      Result := Middle;
      Break;
    end;
  end;
end;

function TIdObjectRegistry.ObjectAt(Index: Integer): TObject;
begin
  Result := Self.ObjectRegistry.Objects[Index];
end;

function TIdObjectRegistry.ObjectCalled(L: TStrings; ObjectID: TRegisteredObjectID): TObject;
var
  Index: Integer;
begin
  Index := Self.IndexOf(L, ObjectID);

  if (Index = ItemNotFoundIndex) then
    Result := nil
  else
    Result := L.Objects[Index];
end;

function TIdObjectRegistry.RegisteredIDOf(O: TObject): TRegisteredObjectID;
begin
  Result := Self.FindID(Self.ObjectRegistry, O);
end;

function TIdObjectRegistry.ReservedIDOf(O: TObject): TRegisteredObjectID;
begin
  Result := Self.FindID(Self.ReservedIDs, O);
end;

procedure TIdObjectRegistry.Unlock;
begin
  Self.RegLock.Release;
end;

function TIdObjectRegistry.UnusedID: TRegisteredObjectID;
begin
  repeat
    Result := TRegisteredObjectID(ConstructUUID);
  until (Self.ObjectCalled(Self.ObjectRegistry, Result) = nil)
    and (Self.ObjectCalled(Self.ReservedIDs,    Result) = nil);
end;

initialization
  GObjectRegistryInstance := TIdObjectRegistry.Create;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GObjectRegistryInstance.Free;
end.
