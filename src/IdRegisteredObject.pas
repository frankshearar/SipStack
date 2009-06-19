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
