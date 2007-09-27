unit IdRegisteredObject;

interface

uses
  Classes, LoGGer, SysUtils;

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

  // I provide facilities to unambiguously locate any (registered) object
  // resident in memory, and to allow these objects to register and unregister
  // from me as they are instantiate and freed.
  //
  // Note that you can register any instance of any kind of object, but if you
  // register an instance that isn't a TIdRegisteredObject then you have to
  // store the object ID yourself.
  TIdObjectRegistry = class(TObject)
  private
    class procedure CollectAllObjectsOfClassOnly(SearchType: TClass; Results: TStrings);
    class procedure CollectAllObjectsOfClassOrSubtype(SearchType: TClass; Results: TStrings);
    class procedure Log(Description: String);
    class function  ObjectAt(Index: Integer): TObject;
    class function  ObjectRegistry: TStrings;
  public
    class procedure CollectAllObjectsOfClass(SearchType: TClass; Results: TStrings; AllowSubclassTypes: Boolean = true);
    class function  FindObject(ObjectID: String): TObject;
    class function  RegisterObject(Instance: TObject): String;
    class procedure SetLogger(Log: TLoGGerThread;
                              LogName: String;
                              SourceRef: Cardinal);
    class procedure UnregisterObject(ObjectID: String);
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
  IdSystem;

const
  ItemNotFoundIndex = -1;

var
  GLog:            TLoGGerThread;
  GLogName:        String;
  GObjectRegistry: TStringList;
  GSourceRef:      Cardinal;

function CreateSortedList: TStringList;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupError;
  Result.Sorted     := true;
end;

//******************************************************************************
//* TIdRegisteredObject                                                        *
//******************************************************************************
//* TIdRegisteredObject Public methods *****************************************

constructor TIdRegisteredObject.Create;
begin
  inherited Create;

  Self.fID := TIdObjectRegistry.RegisterObject(Self);
end;

destructor TIdRegisteredObject.Destroy;
begin
  TIdObjectRegistry.UnregisterObject(Self.ID);

  inherited Destroy;
end;

//******************************************************************************
//* TIdObjectRegistry                                                          *
//******************************************************************************
//* TIdObjectRegistry Public methods *******************************************

class procedure TIdObjectRegistry.CollectAllObjectsOfClass(SearchType: TClass; Results: TStrings; AllowSubclassTypes: Boolean = true);
begin
  Results.Clear;

  if AllowSubclassTypes then
    Self.CollectAllObjectsOfClassOrSubtype(SearchType, Results)
  else
    Self.CollectAllObjectsOfClassOnly(SearchType, Results);
end;

class function TIdObjectRegistry.FindObject(ObjectID: String): TObject;
var
  Index: Integer;
begin
  Index := Self.ObjectRegistry.IndexOf(ObjectID);

  if (Index = ItemNotFoundIndex) then
    Result := nil
  else
    Result := Self.ObjectAt(Index);
end;

class function TIdObjectRegistry.RegisterObject(Instance: TObject): String;
begin
  repeat
    Result := ConstructUUID;
  until (Self.ObjectRegistry.IndexOf(Result) = ItemNotFoundIndex);

  Self.ObjectRegistry.AddObject(Result, Instance);
  Self.Log(Format(RegisterLogMsg, [Instance.ClassName, Result]));
end;

class procedure TIdObjectRegistry.SetLogger(Log: TLoGGerThread;
                                            LogName: String;
                                            SourceRef: Cardinal);
begin
  GLog       := Log;
  GLogName   := LogName;
  GSourceRef := SourceRef;
end;

class procedure TIdObjectRegistry.UnregisterObject(ObjectID: String);
var
  Index: Integer;
begin
  Index := Self.ObjectRegistry.IndexOf(ObjectID);
  if (Index <> ItemNotFoundIndex) then begin
    Self.Log(Format(UnregisterLogMsg, [Self.ObjectRegistry.Objects[Index].ClassName, ObjectID]));
    Self.ObjectRegistry.Delete(Index);
  end;
end;

//* TIdObjectRegistry Private methods ******************************************

class procedure TIdObjectRegistry.CollectAllObjectsOfClassOnly(SearchType: TClass; Results: TStrings);
var
  I: Integer;
begin
  Results.Clear;

  for I := 0 to Self.ObjectRegistry.Count - 1 do begin
    if (Self.ObjectAt(I).ClassType = SearchType) then
      Results.AddObject(Self.ObjectRegistry[I],
                        Self.ObjectRegistry.Objects[I]);
  end;
end;

class procedure TIdObjectRegistry.CollectAllObjectsOfClassOrSubtype(SearchType: TClass; Results: TStrings);
var
  I: Integer;
begin
  Results.Clear;

  for I := 0 to Self.ObjectRegistry.Count - 1 do begin
    if (Self.ObjectAt(I) is SearchType) then
      Results.AddObject(Self.ObjectRegistry[I],
                        Self.ObjectRegistry.Objects[I]);
  end;
end;

class procedure TIdObjectRegistry.Log(Description: String);
begin
  if not Assigned(GLog) then Exit;

  GLog.Lock;
  try
    if not GLog.Logs.LogExists(GLogName) then Exit;
  finally
    GLog.Unlock;
  end;

  GLog.Write(GLogName, LoGGerVerbosityLevelDebug, GSourceRef, '', 0, Description, '');
end;

class function TIdObjectRegistry.ObjectAt(Index: Integer): TObject;
begin
  Result := Self.ObjectRegistry.Objects[Index];
end;

class function TIdObjectRegistry.ObjectRegistry: TStrings;
begin
  Result := GObjectRegistry;
end;

initialization
  GObjectRegistry := CreateSortedList;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GObjectRegistry.Free;
end.
