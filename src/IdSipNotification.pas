{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipNotification;

interface

uses
  Classes, SyncObjs;

type
  // I represent a reified method call.
  // My subclasses have properties representing the parameters of the method
  // call, and the Run method the actually executes the method by typecasting
  // its Subject parameter.
  TIdSipMethod = class(TObject)
  public
    procedure Run(const Subject: IInterface); virtual;
  end;

  // I provide a storage space for listeners, and you use Notify to make me
  // iterate over my contained listeners, passing each item to the
  // TIdSipMethod.
  TIdSipNotificationList = class(TObject)
  private
    List: TList;
    Lock: TCriticalSection;

    procedure CopyList(Copy: TList);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddListener(L: IInterface);
    function  Count: Integer;
    procedure Notify(Method: TIdSipMethod);
    procedure RemoveListener(L: IInterface);
  end;

implementation

//******************************************************************************
//* TIdSipMethod                                                               *
//******************************************************************************
//* TIdSipMethod Public methods ************************************************

procedure TIdSipMethod.Run(const Subject: IInterface);
begin
  // By default do nothing
end;

//******************************************************************************
//* TIdSipNotificationList                                                     *
//******************************************************************************
//* TIdSipNotificationList Public methods **************************************

constructor TIdSipNotificationList.Create;
begin
  inherited Create;

  Self.List := TList.Create;
  Self.Lock := TCriticalSection.Create;
end;

destructor TIdSipNotificationList.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.List.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdSipNotificationList.AddListener(L: IInterface);
begin
  Self.Lock.Acquire;
  try
    Self.List.Add(Pointer(L));
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipNotificationList.Count: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := Self.List.Count;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdSipNotificationList.Notify(Method: TIdSipMethod);
var
  Copy: TList;
  I:    Integer;
begin
  Copy := TList.Create;
  try
    Self.Lock.Acquire;
    try
      Self.CopyList(Copy);
    finally
      Self.Lock.Release;
    end;

    for I := 0 to Copy.Count - 1 do
      try
        Method.Run(IInterface(Copy[I]));
      except
      end;
  finally
    Copy.Free;
  end;
end;

procedure TIdSipNotificationList.RemoveListener(L: IInterface);
begin
  Self.Lock.Acquire;
  try
    Self.List.Remove(Pointer(L));
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdSipNotificationList.CopyList(Copy: TList);
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

end.
