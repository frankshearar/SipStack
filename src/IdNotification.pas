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
  Classes, SyncObjs;

type
  // I represent a reified method call.
  // My subclasses have properties representing the parameters of the method
  // call, and the Run method the actually executes the method by typecasting
  // its Subject parameter.
  TIdMethod = class(TObject)
  public
    procedure Run(const Subject: IInterface); virtual;
  end;

  // I provide a storage space for listeners, and you use Notify to make me
  // iterate over my contained listeners, passing each item to the
  // TIdMethod.
  TIdNotificationList = class(TPersistent)
  private
    ExpectedExceptions:     TList;
    ExpectedExceptionsLock: TCriticalSection;
    List:                   TList;
    Lock:                   TCriticalSection;

    procedure CopyList(Copy: TList);
    function  Expects(ExceptionType: TClass): Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddExpectedException(ExceptionType: TClass);
    procedure Assign(Src: TPersistent); override;
    procedure AddListener(const L: IInterface);
    function  Count: Integer;
    procedure Notify(Method: TIdMethod);
    procedure RemoveListener(const L: IInterface);
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdMethod                                                                  *
//******************************************************************************
//* TIdMethod Public methods ***************************************************

procedure TIdMethod.Run(const Subject: IInterface);
begin
  // By default do nothing
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
  Self.List                   := TList.Create;
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

procedure TIdNotificationList.Assign(Src: TPersistent);
var
  I:     Integer;
  Other: TIdNotificationList;
begin
  if (Src is TIdNotificationList) then begin
    Other := Src as TIdNotificationList;
    Self.Lock.Acquire;
    try
      Self.List.Clear;

      Other.Lock.Acquire;
      try
        for I := 0 to Other.List.Count - 1 do
          Self.List.Add(Other.List[I]);
      finally
        Other.Lock.Release;
      end;
    finally
      Self.Lock.Release;
    end
  end
  else
    inherited Assign(Src);
end;

procedure TIdNotificationList.AddListener(const L: IInterface);
begin
  if not Assigned(L) then Exit;

  Self.Lock.Acquire;
  try
    Self.List.Add(Pointer(L));
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

procedure TIdNotificationList.Notify(Method: TIdMethod);
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.CopyList(Copy);

    for I := 0 to Copy.Count - 1 do
      try
        Method.Run(IInterface(Copy[I]));
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
    Self.List.Remove(Pointer(L));
  finally
    Self.Lock.Release;
  end;
end;

//* TIdNotificationList Private methods ****************************************

procedure TIdNotificationList.CopyList(Copy: TList);
var
  I: Integer;
begin
  Copy.Clear;
  Self.Lock.Acquire;
  try
    for I := 0 to Self.List.Count - 1 do
      Copy.Add(Self.List[I]);
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

end.
