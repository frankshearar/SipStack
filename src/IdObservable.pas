unit IdObservable;

interface

uses
  Classes, IdInterfacedObject, SyncObjs;

type
  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // Unfortunately I never know the type of Observed and have to typecast
  // the Observed, but c'est la vie.
  IIdSipObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(Observed: TObject);
  end;

  TIdObservable = class(TIdInterfacedObject)
  private
    Observers:    TList;
    ObserverLock: TCriticalSection;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdSipObserver);
    procedure NotifyListenersOfChange;
    function  ObserverCount: Integer;
    procedure RemoveObserver(const Listener: IIdSipObserver);
  end;

implementation

//******************************************************************************
//* TIdObservable                                                              *
//******************************************************************************
//* TIdObservable Public methods ***********************************************

constructor TIdObservable.Create;
begin
  inherited Create;

  Self.Observers    := TList.Create;
  Self.ObserverLock := TCriticalSection.Create;
end;

destructor TIdObservable.Destroy;
begin
  Self.ObserverLock.Free;
  Self.Observers.Free;

  inherited Destroy;
end;

procedure TIdObservable.AddObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Add(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdObservable.NotifyListenersOfChange;
var
  I:    Integer;
  Copy: TList;
begin
  // We copy the list because the observers could choose to remove themselves
  // as listeners from Self.

  Self.ObserverLock.Create;
  try
    Copy := TList.Create;
    try
      for I := 0 to Self.Observers.Count - 1 do
        Copy.Add(Self.Observers[I]);

      for I := 0 to Copy.Count - 1 do
        IIdSipObserver(Copy[I]).OnChanged(Self);
    finally
      Copy.Free;
    end;

  finally
    Self.ObserverLock.Release;
  end;
end;

function TIdObservable.ObserverCount: Integer;
begin
  Self.ObserverLock.Acquire;
  try
    Result := Self.Observers.Count;
  finally
    Self.ObserverLock.Release;
  end;
end;

procedure TIdObservable.RemoveObserver(const Listener: IIdSipObserver);
begin
  Self.ObserverLock.Acquire;
  try
    Self.Observers.Remove(Pointer(Listener));
  finally
    Self.ObserverLock.Release;
  end;
end;

end.
