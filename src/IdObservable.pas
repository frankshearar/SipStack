{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdObservable;

interface

uses
  Classes, IdInterfacedObject, IdNotification, SyncObjs;

type
  // I watch other objects for changes. When they change (in their arbitrary
  // fashion) they tell me, and I update my own state accordingly.
  // Unfortunately I never know the type of Observed and have to typecast
  // the Observed, but c'est la vie.
  IIdObserver = interface
    ['{665CFE94-8EFD-4710-A5CC-ED01BCF7961E}']
    procedure OnChanged(Observed: TObject);
  end;

  // I provide the basic functionality necessary to maintain a list of
  // Observers.
  TIdObservable = class(TIdInterfacedObject)
  private
    Observers: TIdNotificationList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddObserver(const Listener: IIdObserver);
    procedure NotifyListenersOfChange; overload;
    procedure NotifyListenersOfChange(Data: TObject); overload;
    function  ObserverCount: Integer;
    procedure RemoveObserver(const Listener: IIdObserver);
  end;

  // I represent a reified Method that you use to tell listeners when
  // Subject has changed in some way.
  TIdObserverChangedMethod = class(TIdNotification)
  private
    fObserved: TObject;
  public
    procedure Run(const Subject: IInterface); override;

    property Observed: TObject read fObserved write fObserved;
  end;

  // Use me for debugging purposes.
  TIdObserverListener = class(TIdInterfacedObject,
                              IIdObserver)
  private
    fChanged: Boolean;
    fData:    TObject;
  public
    constructor Create; override;

    procedure OnChanged(Observed: TObject);

    property Changed: Boolean read fChanged;
    property Data:    TObject read fData;
  end;

implementation

//******************************************************************************
//* TIdObservable                                                              *
//******************************************************************************
//* TIdObservable Public methods ***********************************************

constructor TIdObservable.Create;
begin
  inherited Create;

  Self.Observers := TIdNotificationList.Create;
end;

destructor TIdObservable.Destroy;
begin
  Self.Observers.Free;

  inherited Destroy;
end;

procedure TIdObservable.AddObserver(const Listener: IIdObserver);
begin
  Self.Observers.AddListener(Listener);
end;

procedure TIdObservable.NotifyListenersOfChange;
begin
  Self.NotifyListenersOfChange(Self);
end;

procedure TIdObservable.NotifyListenersOfChange(Data: TObject);
var
  Notification: TIdObserverChangedMethod;
begin
  Notification := TIdObserverChangedMethod.Create;
  try
    Notification.Observed := Data;

    Self.Observers.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

function TIdObservable.ObserverCount: Integer;
begin
  Result := Self.Observers.Count;
end;

procedure TIdObservable.RemoveObserver(const Listener: IIdObserver);
begin
  Self.Observers.RemoveListener(Listener);
end;

//******************************************************************************
//* TIdObserverChangedMethod                                                   *
//******************************************************************************
//* TIdObserverChangedMethod Public methods ************************************

procedure TIdObserverChangedMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdObserver).OnChanged(Self.Observed);
end;

//******************************************************************************
//* TIdObserverListener                                                        *
//******************************************************************************
//* TIdObserverListener Public methods *****************************************

constructor TIdObserverListener.Create;
begin
  inherited Create;

  Self.fChanged := false;
  Self.fData    := nil;
end;

procedure TIdObserverListener.OnChanged(Observed: TObject);
begin
  Self.fChanged := true;
  Self.fData    := Observed;
end;

end.
