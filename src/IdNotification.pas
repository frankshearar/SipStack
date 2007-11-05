{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdNotification;

interface

uses
  Classes, Contnrs, SyncObjs;

type
  // I represent a reified method call.
  // My subclasses have properties representing the parameters of the method
  // call, and the Run method actually executes the method by typecasting its
  // Subject parameter.
  TIdNotification = class(TObject)
  public
    procedure Run(const Subject: IInterface); virtual;
  end;

  TIdNotificationRecord = class(TObject)
  private
    fListener: Pointer;
    fPriority: Integer;

    procedure SetListener(Value: IInterface);
    function  GetListener: IInterface;
  public
    property Listener: IInterface read GetListener write SetListener;
    property Priority: Integer    read fPriority write fPriority;
  end;

  // I provide a storage space for listeners, and you use Notify to make me
  // iterate over my contained listeners, passing each item to the
  // TIdNotification.
  TIdNotificationList = class(TPersistent)
  private
    ExpectedExceptions:     TList;
    ExpectedExceptionsLock: TCriticalSection;
    List:                   TObjectList;
    Lock:                   TCriticalSection;

    procedure CopyList(Copy: TObjectList);
    function  Expects(ExceptionType: TClass): Boolean;
    function  Find(Listener: IInterface): TIdNotificationRecord;
    function  GetListeners(Index: Integer): IInterface;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddExpectedException(ExceptionType: TClass);
    procedure Add(Listeners: TIdNotificationList);
    procedure Assign(Src: TPersistent); override;
    procedure AddListener(const L: IInterface; Priority: Integer = 0);
    function  Count: Integer;
    procedure Notify(Method: TIdNotification);
    procedure RemoveListener(const L: IInterface);

    property Listeners[Index: Integer]: IInterface read GetListeners; default;
  end;

const
  FirstNotified = High(Integer);
  LastNotified  = Low(Integer);

function TIdNotificationRecordCompare(Item1, Item2: Pointer): Integer;

implementation

uses
  SysUtils;

const
  ItemNotFoundIndex = -1;

function TIdNotificationRecordCompare(Item1, Item2: Pointer): Integer;
var
  Listener1, Listener2: TIdNotificationRecord;
begin
  Listener1 := TIdNotificationRecord(Item1);
  Listener2 := TIdNotificationRecord(Item2);

  if (Listener1.Priority > Listener2.Priority) then
    Result := -1
  else if (Listener1.Priority < Listener2.Priority) then
    Result := 1
  else
    Result := 0;
end;

//******************************************************************************
//* TIdNotification                                                            *
//******************************************************************************
//* TIdNotification Public methods *********************************************

procedure TIdNotification.Run(const Subject: IInterface);
begin
  // By default do nothing
end;

//******************************************************************************
//* TIdNotificationRecord                                                      *
//******************************************************************************
//* TIdNotificationRecord Private methods **************************************

function TIdNotificationRecord.GetListener: IInterface;
begin
  Result := IInterface(Self.fListener);
end;

procedure TIdNotificationRecord.SetListener(Value: IInterface);
begin
  Self.fListener := Pointer(Value);
end;

//******************************************************************************
//* TIdNotificationList                                                        *
//******************************************************************************
//* TIdNotificationList Public methods *****************************************

constructor TIdNotificationList.Create;
begin
  inherited Create;

  Self.ExpectedExceptions     := TList.Create;
  Self.ExpectedExceptionsLock := TCriticalSection.Create;
  Self.List                   := TObjectList.Create(true);
  Self.Lock                   := TCriticalSection.Create;
end;

destructor TIdNotificationList.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.List.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  Self.ExpectedExceptionsLock.Acquire;
  try
    Self.ExpectedExceptions.Free;
  finally
    Self.ExpectedExceptionsLock.Release;
  end;
  Self.ExpectedExceptionsLock.Free;

  inherited Destroy;
end;

procedure TIdNotificationList.AddExpectedException(ExceptionType: TClass);
begin
  Self.ExpectedExceptionsLock.Acquire;
  try
    Self.ExpectedExceptions.Add(Pointer(ExceptionType));
  finally
    Self.ExpectedExceptionsLock.Release;
  end;
end;

procedure TIdNotificationList.Add(Listeners: TIdNotificationList);
var
  I:      Integer;
  OtherL: TIdNotificationRecord;
begin
  Self.Lock.Acquire;
  try
    Listeners.Lock.Acquire;
    try
      for I := 0 to Listeners.List.Count - 1 do begin
        OtherL := Listeners.List[I] as TIdNotificationRecord;
        Self.AddListener(OtherL.Listener, OtherL.Priority);
      end;
    finally
      Listeners.Lock.Release;
    end;
  finally
    Self.Lock.Release;
  end
end;

procedure TIdNotificationList.Assign(Src: TPersistent);
var
  Other: TIdNotificationList;
begin
  if (Src is TIdNotificationList) then begin
    Other := Src as TIdNotificationList;

    Self.Lock.Acquire;
    try
      Self.List.Clear;
      Self.Add(Other);
    finally
      Self.Lock.Release;
    end
  end
  else
    inherited Assign(Src);
end;

procedure TIdNotificationList.AddListener(const L: IInterface; Priority: Integer = 0);
var
  NewL: TIdNotificationRecord;
begin
  if not Assigned(L) then Exit;

  Self.Lock.Acquire;
  try
    NewL := TIdNotificationRecord.Create;
    NewL.Listener := L;
    NewL.Priority := Priority;
    Self.List.Add(NewL);
    Self.List.Sort(TIdNotificationRecordCompare);
  finally
    Self.Lock.Release;
  end;
end;

function TIdNotificationList.Count: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := Self.List.Count;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdNotificationList.Notify(Method: TIdNotification);
var
  Copy: TObjectList;
  I:    Integer;
begin
  // Create a copy of the listeners, and iterate over them. Should a listener
  // raise an expected exception, ignore that exception.
  // Why a copy? Listeners can do arbitrary things, like removing themselves as
  // listeners. Iterating over a mutable collection is a Bad Idea.
  Copy := TObjectList.Create(true);
  try
    Self.CopyList(Copy);

    for I := 0 to Copy.Count - 1 do
      try
        Method.Run(TIdNotificationRecord(Copy[I]).Listener);
      except
        on E: Exception do
          if not Self.Expects(E.ClassType) then raise;
      end;
  finally
    Copy.Free;
  end;
end;

procedure TIdNotificationList.RemoveListener(const L: IInterface);
begin
  Self.Lock.Acquire;
  try
    Self.List.Remove(Self.Find(L));
  finally
    Self.Lock.Release;
  end;
end;

//* TIdNotificationList Private methods ****************************************

procedure TIdNotificationList.CopyList(Copy: TObjectList);
var
  I: Integer;
  NewL: TIdNotificationRecord;
begin
  Copy.Clear;
  Self.Lock.Acquire;
  try
    for I := 0 to Self.List.Count - 1 do begin
      NewL := TIdNotificationRecord.Create;
      NewL.Listener := TIdNotificationRecord(Self.List[I]).Listener;
      NewL.Priority := TIdNotificationRecord(Self.List[I]).Priority;
      Copy.Add(NewL);
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdNotificationList.Expects(ExceptionType: TClass): Boolean;
var
  I: Integer;
begin
  Result := false;

  Self.ExpectedExceptionsLock.Acquire;
  try
    I := 0;
    while (I < Self.ExpectedExceptions.Count) and not Result do begin
      Result := TClass(Self.ExpectedExceptions[I]) = ExceptionType;
      Inc(I);
    end;
  finally
    Self.ExpectedExceptionsLock.Release;
  end;
end;

function TIdNotificationList.Find(Listener: IInterface): TIdNotificationRecord;
var
  I: Integer;
  L: TIdNotificationRecord;
begin
  // Precondition: you've acquired Self.Lock.

  Result := nil;

  for I := 0 to Self.List.Count - 1 do begin
    L := TIdNotificationRecord(Self.List[I]);

    if (L.Listener = Listener) then begin
      Result := L;
      Break;
    end;
  end;
end;

function TIdNotificationList.GetListeners(Index: Integer): IInterface;
begin
  Self.Lock.Acquire;
  try
    Result := TIdNotificationRecord(Self.List[Index]).Listener;
  finally
    Self.Lock.Release;
  end;
end;

end.
