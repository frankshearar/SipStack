unit IdRTPTimerQueue;

interface

uses
  Classes, Contnrs, IdThread, SyncObjs;

type
  TIdRTPWait = class(TObject)
    fTriggerTime: Cardinal;
  public
    property TriggerTime: Cardinal read fTriggerTime write fTriggerTime;

    function  Due: Boolean;
    function  TimeToWait: Cardinal;
    procedure Trigger; virtual; abstract;
  end;

  TIdRTPEventWait = class(TIdRTPWait)
  private
    fEvent: TEvent;
  public
    property Event: TEvent read fEvent write fEvent;

    procedure Trigger; override;
  end;

  TIdRTPNotifyEventWait = class(TIdRTPWait)
  private
    fEvent: TNotifyEvent;
  public
    property Event: TNotifyEvent read fEvent write fEvent;

    procedure Trigger; override;
  end;

  // I supply a single timer thread with which you may register a wait time
  // (in milliseconds) and an event (a TEvent or a TNotifyEvent). When that
  // wait time is up, I set the event you've given me. You may register as
  // many events as you like, up to the size of a TObjectList (High(Integer)).
  // 
  // You may safely specify a wait time of up to 1 day (86 400 000 ms).
  TIdRTPTimerQueue = class(TIdThread)
  private
    EventList: TObjectList;
    Lock:      TCriticalSection;

    procedure Add(MillisecsWait: Cardinal; Event: TIdRTPWait);
    function  EventAt(Index: Integer): TIdRTPWait;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TEvent); overload;
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TNotifyEvent); overload;
    function  EarliestEvent: TIdRTPWait;
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
  end;

implementation

uses
  IdGlobal, IdRTP, SysUtils;

//******************************************************************************
//* TIdRTPWait                                                                 *
//******************************************************************************
//* TIdRTPWait Public methods **************************************************

function TIdRTPWait.Due: Boolean;
begin
  Result := GetTickCount >= Self.TriggerTime;
end;

function TIdRTPWait.TimeToWait: Cardinal;
begin
  if Self.Due then
    Result := 0
  else
    Result := Self.TriggerTime - GetTickCount;
end;

//******************************************************************************
//* TIdRTPEventWait                                                            *
//******************************************************************************
//* TIdRTPEventWait Public methods *********************************************

procedure TIdRTPEventWait.Trigger;
begin
  Self.Event.SetEvent;
end;

//******************************************************************************
//* TIdRTPNotifyEventWait                                                      *
//******************************************************************************
//* TIdRTPNotifyEventWait Public methods ***************************************

procedure TIdRTPNotifyEventWait.Trigger;
begin
  if Assigned(Self.Event) then
    Self.Event(Self);
end;

//******************************************************************************
//* TIdRTPTimerQueue                                                           *
//******************************************************************************
//* TIdRTPTimerQueue Public methods ********************************************

constructor TIdRTPTimerQueue.Create(ACreateSuspended: Boolean = True);
begin
  inherited Create(ACreateSuspended);

  Self.EventList := TObjectList.Create(true);
  Self.Lock      := TCriticalSection.Create;
end;

destructor TIdRTPTimerQueue.Destroy;
begin
  Self.EventList.Free;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdRTPTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                    Event: TEvent);
var
  EventWait: TIdRTPEventWait;
begin
  EventWait := TIdRTPEventWait.Create;
  try
    EventWait.Event := Event;
    Self.Add(MillisecsWait, EventWait);
  except
    if (Self.EventList.IndexOf(EventWait) <> -1) then
      Self.EventList.Remove(EventWait)
    else
      FreeAndNil(EventWait);
    raise;
  end;
end;

procedure TIdRTPTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                    Event: TNotifyEvent);
var
  EventWait: TIdRTPNotifyEventWait;
begin
  Self.Lock.Acquire;
  try
    EventWait := TIdRTPNotifyEventWait.Create;
    try
      EventWait.Event := Event;
      Self.Add(MillisecsWait, EventWait);
    except
    if (Self.EventList.IndexOf(EventWait) <> -1) then
      Self.EventList.Remove(EventWait)
    else
      FreeAndNil(EventWait);
      raise;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdRTPTimerQueue.EarliestEvent: TIdRTPWait;
var
  I: Integer;
begin
  if (Self.EventList.Count = 0) then begin
    Result := nil;
    Exit;
  end;

  Self.Lock.Acquire;
  try
    Result := Self.EventAt(0);
    for I := 1 to Self.EventList.Count - 1 do
      if Self.Before(Self.EventAt(I).TriggerTime, Result.TriggerTime) then
        Result := Self.EventAt(I);
  finally
    Self.Lock.Release;
  end;
end;

function TIdRTPTimerQueue.Before(TimeA,
                                 TimeB: Cardinal): Boolean;
begin
  // You can't use Before() to determine the ordering of ticks
  // if the ticks are more than a day apart (i.e., MSecsPerDay ticks).
  Result := GetTickDiff(TimeA, TimeB) < MSecsPerDay;
end;

//* TIdRTPTimerQueue Protected methods *****************************************

procedure TIdRTPTimerQueue.Run;
var
  NextEvent: TIdRTPWait;
begin
  while not Terminated do begin
    NextEvent := Self.EarliestEvent;

    if Assigned(NextEvent) then begin
      if not NextEvent.Due then
        IdGlobal.Sleep(NextEvent.TimeToWait);

      NextEvent.Trigger;
      Self.EventList.Remove(NextEvent)
    end
    else
      IdGlobal.Sleep(100);
  end;
end;

//* TIdRTPTimerQueue Private methods *******************************************

procedure TIdRTPTimerQueue.Add(MillisecsWait: Cardinal; Event: TIdRTPWait);
begin
  Self.Lock.Acquire;
  try
    try
      Self.EventList.Add(Event);
      Event.TriggerTime := AddModulo(GetTickCount,
                                     MillisecsWait,
                                     High(MillisecsWait));
    except
      Self.EventList.Remove(Event);
      raise;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdRTPTimerQueue.EventAt(Index: Integer): TIdRTPWait;
begin
  Result := Self.EventList[Index] as TIdRTPWait;
end;

end.
