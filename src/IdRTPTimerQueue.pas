unit IdRTPTimerQueue;

interface

uses
  Classes, Contnrs, IdThread, SyncObjs;

type
  TIdRTPWait = class(TObject)
    fTriggerTime: Cardinal;
  public
    function  Due: Boolean;
    function  TimeToWait: Cardinal;
    procedure Trigger; virtual; abstract;

    property TriggerTime: Cardinal read fTriggerTime write fTriggerTime;    
  end;

  TIdRTPEventWait = class(TIdRTPWait)
  private
    fEvent: TEvent;
  public
    procedure Trigger; override;

    property Event: TEvent read fEvent write fEvent;
  end;

  TIdRTPNotifyEventWait = class(TIdRTPWait)
  private
    fEvent: TNotifyEvent;
  public
    procedure Trigger; override;

    property Event: TNotifyEvent read fEvent write fEvent;
  end;

  // I supply a single timer thread with which you may register a wait time
  // (in milliseconds) and an event (a TEvent or a TNotifyEvent). When that
  // wait time is up, I set the event you've given me. You may register as
  // many events as you like, up to the limits of a TObjectList (High(Integer)).
  //
  // You may safely specify a wait time of up to 1 day (86 400 000 ms).
  //
  // WaitEvent lets me re-evaluate the shortest wait time whenever something
  //  adds or removes events. Or when something's Terminated me.
  TIdRTPTimerQueue = class(TIdThread)
  private
    EventList: TObjectList;
    Lock:      TCriticalSection;
    WaitEvent: TEvent;

    procedure Add(MillisecsWait: Cardinal;
                  Event: TIdRTPWait);
    function  EarliestEvent: TIdRTPWait;
    function  EventAt(Index: Integer): TIdRTPWait;
    function  ShortestWait: Cardinal;
    procedure TriggerEarliestEvent;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TEvent); overload;
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TNotifyEvent); overload;
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
    procedure RemoveEvent(Event: TEvent); overload;
    procedure RemoveEvent(Event: TNotifyEvent); overload;
    procedure Terminate; override;
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
  if Assigned(Self.Event) then
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
  // Before inherited - inherited creates the actual thread and if not
  // suspended will start before we initialize.
  Self.EventList := TObjectList.Create(true);
  Self.Lock      := TCriticalSection.Create;
  Self.WaitEvent := TSimpleEvent.Create;

  inherited Create(ACreateSuspended);
end;

destructor TIdRTPTimerQueue.Destroy;
begin
  Self.WaitEvent.Free;

  Self.Lock.Acquire;
  try
    Self.EventList.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdRTPTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                    Event: TEvent);
var
  EventWait: TIdRTPEventWait;
begin
  // Add will free EventWait in the event of an exception
  EventWait := TIdRTPEventWait.Create;
  EventWait.Event := Event;
  Self.Add(MillisecsWait, EventWait);
end;

procedure TIdRTPTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                    Event: TNotifyEvent);
var
  EventWait: TIdRTPNotifyEventWait;
begin
  // Add will free EventWait in the event of an exception
  EventWait := TIdRTPNotifyEventWait.Create;
  EventWait.Event := Event;
  Self.Add(MillisecsWait, EventWait);
end;

function TIdRTPTimerQueue.Before(TimeA,
                                 TimeB: Cardinal): Boolean;
begin
  // You can't use Before() to determine the ordering of ticks
  // if the ticks are more than a day apart (i.e., MSecsPerDay ticks).
  Result := GetTickDiff(TimeA, TimeB) < MSecsPerDay;
end;

procedure TIdRTPTimerQueue.RemoveEvent(Event: TEvent);
var
  I: Integer;
begin
  Self.Lock.Acquire;
  try
    I := 0;
    while (I < Self.EventList.Count) do
      if (Self.EventAt(I) is TIdRTPEventWait)
        and ((Self.EventAt(I) as TIdRTPEventWait).Event = Event) then
        Self.EventList.Delete(I)
      else
        Inc(I);
  finally
    Self.Lock.Release;
  end;
  Self.WaitEvent.SetEvent;
end;

procedure TIdRTPTimerQueue.RemoveEvent(Event: TNotifyEvent);
var
  I:         Integer;
  ThisEvent: TNotifyEvent;
begin
  Self.Lock.Acquire;
  try
    I := 0;
    while (I < Self.EventList.Count) do
      if (Self.EventAt(I) is TIdRTPNotifyEventWait) then begin
        ThisEvent := (Self.EventAt(I) as TIdRTPNotifyEventWait).Event;
        if (@ThisEvent = @Event) then
          Self.EventList.Delete(I)
      end
      else
        Inc(I);
  finally
    Self.Lock.Release;
  end;
  Self.WaitEvent.SetEvent;
end;

procedure TIdRTPTimerQueue.Terminate;
begin
  inherited Terminate;

  Self.WaitEvent.SetEvent;
end;

//* TIdRTPTimerQueue Protected methods *****************************************

procedure TIdRTPTimerQueue.Run;
begin
  while not Self.Stopped do begin
    Self.WaitEvent.WaitFor(Self.ShortestWait);

    if not Self.Stopped then
      Self.TriggerEarliestEvent;
  end;
end;

//* TIdRTPTimerQueue Private methods *******************************************

procedure TIdRTPTimerQueue.Add(MillisecsWait: Cardinal;
                               Event: TIdRTPWait);
begin
  Self.Lock.Acquire;
  try
    try
      Self.EventList.Add(Event);
      Event.TriggerTime := AddModulo(GetTickCount,
                                     MillisecsWait,
                                     High(MillisecsWait));
    except
      if (Self.EventList.IndexOf(Event) <> -1) then
        Self.EventList.Remove(Event)
      else
        Event.Free;

      raise;
    end;
  finally
    Self.Lock.Release;
  end;
  Self.WaitEvent.SetEvent;
end;

function TIdRTPTimerQueue.EarliestEvent: TIdRTPWait;
var
  I: Integer;
begin
  // Precondition: Something acquired Self.Lock
  if (Self.EventList.Count = 0) then begin
    Result := nil;
    Exit;
  end;

  Result := Self.EventAt(0);
  for I := 1 to Self.EventList.Count - 1 do
    if Self.Before(Self.EventAt(I).TriggerTime, Result.TriggerTime) then
      Result := Self.EventAt(I);
end;

function TIdRTPTimerQueue.EventAt(Index: Integer): TIdRTPWait;
begin
  // Precondition: Something acquired Self.Lock
  Result := Self.EventList[Index] as TIdRTPWait;
end;

function TIdRTPTimerQueue.ShortestWait: Cardinal;
var
  NextEvent: TIdRTPWait;
begin
  Self.Lock.Acquire;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) then
      Result := NextEvent.TimeToWait
    else
      Result := 10000;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdRTPTimerQueue.TriggerEarliestEvent;
var
  NextEvent: TIdRTPWait;
begin
  Self.Lock.Acquire;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) and NextEvent.Due then begin
      NextEvent.Trigger;
      Self.EventList.Remove(NextEvent);
    end;
    
    Self.WaitEvent.ResetEvent;
  finally
    Self.Lock.Release;
  end;
end;

end.
