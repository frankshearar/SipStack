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
  // My subclasses and I automatically register/unregister ourselves to the
  // TIdObjectRegistry.
  TIdRegisteredObject = class(TObject)
  private
    fID: String;
  public
    constructor Create; overload; virtual;
    destructor  Destroy; override;

    property ID: String read fID;
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
  // from me as they are instantiate and freed.
  //
  // Note that you can register any instance of any kind of object, but if you
  // register an instance that isn't a TIdRegisteredObject then you have to
  // store the object ID yourself.
  TIdObjectRegistry = class(TObject)
  private
    ObjectRegistry: TStrings;
    RegLock:        TCriticalSection;

    procedure Collect(SearchType: TClass; SearchBlock: TIdCollectBlock; Results: TStrings);
    procedure Lock;
    procedure Log(Description: String);
    function  ObjectAt(Index: Integer): TObject;
    procedure Unlock;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    class function Singleton: TIdObjectRegistry;

    procedure CollectAllObjectsOfClass(SearchType: TClass; Results: TStrings; AllowSubclassTypes: Boolean = true);
    function  FindObject(ObjectID: String): TObject;
    function  RegisterObject(Instance: TObject): String;
    procedure UnregisterObject(ObjectID: String);
    procedure WithExtantObjectDo(ObjectID: String; Closure: TObjectMethod);
    procedure WithObjectDo(ObjectID: String; Closure: TObjectClosure);
  end;

  // Throw me any time something goes unexpectedly wrong with the object
  // registry - perhaps a given string doesn't point to a registered object, or
  // to an object of an unexpected type.
  ERegistry = class(Exception);

const
  RegisterLogMsg   = '%s with ID %s instantiated';
  UnregisterLogMsg = '%s with ID %s freed';

implementation

uses
  IdSystem, PluggableLogging;

const
  ItemNotFoundIndex = -1;

var
  GObjectRegistryInstance: TIdObjectRegistry;

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
  inherited Create;

  Self.fID := TIdObjectRegistry.Singleton.RegisterObject(Self);
end;

destructor TIdRegisteredObject.Destroy;
begin
  TIdObjectRegistry.Singleton.UnregisterObject(Self.ID);

  inherited Destroy;
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
end;

destructor TIdObjectRegistry.Destroy;
begin
  Self.Lock;
  try
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

function TIdObjectRegistry.FindObject(ObjectID: String): TObject;
var
  Index: Integer;
begin
  // This, and all external objects that use it, have a time-of-use,
  // time-of-check issue. Right now, a TIdObjectRegistry must not be accessible
  // from multiple threads. It is in the SIP stack: transports are referenced
  // both by socket listeners and the stack's timer!).
  //
  // Move your code to use WithObjectDo instead.

  Self.Lock;
  try
    Index := Self.ObjectRegistry.IndexOf(ObjectID);

    if (Index = ItemNotFoundIndex) then
      Result := nil
    else
      Result := Self.ObjectAt(Index);
  finally
    Self.Unlock;
  end;
end;

function TIdObjectRegistry.RegisterObject(Instance: TObject): String;
begin
  Self.Lock;
  try
    repeat
      Result := ConstructUUID;
    until (Self.ObjectRegistry.IndexOf(Result) = ItemNotFoundIndex);

    Self.ObjectRegistry.AddObject(Result, Instance);
    Self.Log(Format(RegisterLogMsg, [Instance.ClassName, Result]));
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.UnregisterObject(ObjectID: String);
var
  Index: Integer;
begin
  Self.Lock;
  try
    Index := Self.ObjectRegistry.IndexOf(ObjectID);
    if (Index <> ItemNotFoundIndex) then begin
      Self.Log(Format(UnregisterLogMsg, [Self.ObjectRegistry.Objects[Index].ClassName, ObjectID]));
      Self.ObjectRegistry.Delete(Index);
    end;
  finally
    Self.Unlock;
  end;
end;

procedure TIdObjectRegistry.WithExtantObjectDo(ObjectID: String; Closure: TObjectMethod);
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

procedure TIdObjectRegistry.WithObjectDo(ObjectID: String; Closure: TObjectClosure);
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

procedure TIdObjectRegistry.Lock;
begin
  Self.RegLock.Acquire;
end;

procedure TIdObjectRegistry.Log(Description: String);
begin
  LogEntry(Description, '', slDebug, 0, '');
end;

function TIdObjectRegistry.ObjectAt(Index: Integer): TObject;
begin
  Result := Self.ObjectRegistry.Objects[Index];
end;

procedure TIdObjectRegistry.Unlock;
begin
  Self.RegLock.Release;
end;

initialization
  GObjectRegistryInstance := TIdObjectRegistry.Create;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GObjectRegistryInstance.Free;
end.
