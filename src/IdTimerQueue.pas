{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdTimerQueue;

interface

uses
  Classes, Contnrs, IdBaseThread, SyncObjs;

type
  // I represent something that will happen in the future. If you want an alarm
  // to go off in 10 seconds, you'd instantiate me (well, a subclass of me) with
  // a TriggerTime of (Now + 10000) (milliseconds) and given me a TEvent or
  // TNotifyEvent or the like to set/run when you call my Trigger.
  // TimeToWait tells you how soon my timer expires.
  //
  // My DebugWaitTime property aids debugging by giving the wait time specified -
  // in our example, a TimerQueue would set DebugWaitTime to 10000, and the
  // TriggerTime to (Now + 10000).
  TIdWait = class(TObject)
  private
    fData:          TObject;
    fDebugWaitTime: Cardinal;
    fTriggerTime:   Cardinal;
  public
    function  Due: Boolean;
    function  MatchEvent(Event: Pointer): Boolean; virtual; abstract;
    function  TimeToWait: Cardinal;
    procedure Trigger; virtual; abstract;

    property Data:          TObject  read fData write fData;
    property DebugWaitTime: Cardinal read fDebugWaitTime write fDebugWaitTime;
    property TriggerTime:   Cardinal read fTriggerTime write fTriggerTime;
  end;

  TIdEventWait = class(TIdWait)
  private
    fEvent: TEvent;
  public
    function  MatchEvent(Event: Pointer): Boolean; override;
    procedure Trigger; override;

    property Event: TEvent read fEvent write fEvent;
  end;

  TIdNotifyEventWait = class(TIdWait)
  private
    fEvent: TNotifyEvent;
  public
    function  MatchEvent(Event: Pointer): Boolean; override;
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
  TIdTimerQueue = class(TIdBaseThread)
  private
    WaitEvent: TEvent;

    procedure Add(MillisecsWait: Cardinal;
                  Event: TIdWait;
                  Data: TObject);
    function  EarliestEvent: TIdWait;
    function  EventAt(Index: Integer): TIdWait;
    procedure InternalRemove(Event: Pointer);
    function  ShortestWait: Cardinal;
    procedure TriggerEarliestEvent;
  protected
    EventList: TObjectList;
    Lock:      TCriticalSection;

    function  IndexOfEvent(Event: Pointer): Integer;
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TEvent;
                       Data: TObject = nil); overload;
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TNotifyEvent;
                       Data: TObject = nil); overload;
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
    procedure RemoveEvent(Event: TEvent); overload;
    procedure RemoveEvent(Event: TNotifyEvent); overload;
    procedure Terminate; override;
  end;

  TIdDebugTimerQueue = class(TIdTimerQueue)
  private
    function HasScheduledEvent(Event: Pointer): Boolean;
  public
    function  EventAt(Index: Integer): TIdWait;
    function  EventCount: Integer;
    procedure LockTimer;
    function  ScheduledEvent(Event: TEvent): Boolean; overload;
    function  ScheduledEvent(Event: TNotifyEvent): Boolean; overload;
    procedure UnlockTimer;
  end;

// Math and conversion functions
function AddModulo(Addend, Augend: Cardinal; Radix: Cardinal): Cardinal;
function AddModuloWord(Addend, Augend: Word): Word;

implementation

uses
  IdGlobal, SysUtils;

const
  DefaultSleepTime      = 10000;
  NotFoundSentinelValue = -1;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function AddModulo(Addend, Augend: Cardinal; Radix: Cardinal): Cardinal;
begin
  Result := (Int64(Addend) + Augend) mod Radix
end;

function AddModuloWord(Addend, Augend: Word): Word;
begin
  Result := AddModulo(Addend, Augend, High(Addend));
end;

//******************************************************************************
//* TIdWait                                                                    *
//******************************************************************************
//* TIdWait Public methods *****************************************************

function TIdWait.Due: Boolean;
begin
  Result := GetTickCount >= Self.TriggerTime;
end;

function TIdWait.TimeToWait: Cardinal;
begin
  if Self.Due then
    Result := 0
  else
    Result := Self.TriggerTime - GetTickCount;
end;

//******************************************************************************
//* TIdEventWait                                                               *
//******************************************************************************
//* TIdEventWait Public methods ************************************************

function TIdEventWait.MatchEvent(Event: Pointer): Boolean;
begin
  Result := Self.Event = Event;
end;

procedure TIdEventWait.Trigger;
begin
  if Assigned(Self.Event) then
    Self.Event.SetEvent;
end;

//******************************************************************************
//* TIdNotifyEventWait                                                         *
//******************************************************************************
//* TIdNotifyEventWait Public methods ******************************************

function TIdNotifyEventWait.MatchEvent(Event: Pointer): Boolean;
begin
  Result := @Self.Event = Event;
end;

procedure TIdNotifyEventWait.Trigger;
begin
  if Assigned(Self.Event) then
    Self.Event(Self);
end;

//******************************************************************************
//* TIdTimerQueue                                                              *
//******************************************************************************
//* TIdTimerQueue Public methods ***********************************************

constructor TIdTimerQueue.Create(ACreateSuspended: Boolean = True);
begin
  inherited Create(false);

  // Before inherited - inherited creates the actual thread and if not
  // suspended will start before we initialize.
  Self.EventList := TObjectList.Create(true);
  Self.Lock      := TCriticalSection.Create;
  Self.WaitEvent := TSimpleEvent.Create;

  Self.FreeOnTerminate := true;

  if not ACreateSuspended then
    Self.Resume;
end;

destructor TIdTimerQueue.Destroy;
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

procedure TIdTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                 Event: TEvent;
                                 Data: TObject = nil);
var
  EventWait: TIdEventWait;
begin
  // Add will free EventWait in the event of an exception
  EventWait := TIdEventWait.Create;
  EventWait.Event := Event;
  Self.Add(MillisecsWait, EventWait, Data);
end;

procedure TIdTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                 Event: TNotifyEvent;
                                 Data: TObject = nil);
var
  EventWait: TIdNotifyEventWait;
begin
  // Add will free EventWait in the event of an exception
  EventWait := TIdNotifyEventWait.Create;
  EventWait.Event := Event;
  Self.Add(MillisecsWait, EventWait, Data);
end;

function TIdTimerQueue.Before(TimeA,
                              TimeB: Cardinal): Boolean;
begin
  // You can't use Before() to determine the ordering of ticks
  // if the ticks are more than a day apart (i.e., MSecsPerDay ticks).
  Result := GetTickDiff(TimeA, TimeB) < MSecsPerDay;
end;

procedure TIdTimerQueue.RemoveEvent(Event: TEvent);
begin
  Self.InternalRemove(Event);
end;

procedure TIdTimerQueue.RemoveEvent(Event: TNotifyEvent);
begin
  Self.InternalRemove(@Event);
end;

procedure TIdTimerQueue.Terminate;
begin
  inherited Terminate;

  Self.WaitEvent.SetEvent;
end;

//* TIdTimerQueue Protected methods ********************************************

function TIdTimerQueue.IndexOfEvent(Event: Pointer): Integer;
var
  Found: Boolean;
begin
  Found := false;
  Self.Lock.Acquire;
  try
    Result := 0;
    while (Result < Self.EventList.Count) and not Found do begin
      if Self.EventAt(Result).MatchEvent(Event) then
        Found := true
      else
        Inc(Result);
    end;

    if not Found or (Result >= Self.EventList.Count) then
      Result := NotFoundSentinelValue;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdTimerQueue.Run;
begin
  while not Self.Terminated do begin
    Self.WaitEvent.WaitFor(Self.ShortestWait);

    if not Self.Terminated then
      Self.TriggerEarliestEvent;
  end;
end;

//* TIdTimerQueue Private methods **********************************************

procedure TIdTimerQueue.Add(MillisecsWait: Cardinal;
                            Event: TIdWait;
                            Data: TObject);
begin
  Self.Lock.Acquire;
  try
    try
      Self.EventList.Add(Event);
      Event.Data          := Data;
      Event.DebugWaitTime := MillisecsWait;
      Event.TriggerTime   := AddModulo(GetTickCount,
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

function TIdTimerQueue.EarliestEvent: TIdWait;
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

function TIdTimerQueue.EventAt(Index: Integer): TIdWait;
begin
  // Precondition: Something acquired Self.Lock
  Result := Self.EventList[Index] as TIdWait;
end;

procedure TIdTimerQueue.InternalRemove(Event: Pointer);
var
  I: Integer;
begin
  // This removes ALL matching wait events matching Event.
  Self.Lock.Acquire;
  try
    I := 0;
    while (I < Self.EventList.Count) do
      if Self.EventAt(I).MatchEvent(Event) then
          Self.EventList.Delete(I)
        else
          Inc(I);
  finally
    Self.Lock.Release;
  end;
  Self.WaitEvent.SetEvent;
end;

function TIdTimerQueue.ShortestWait: Cardinal;
var
  NextEvent: TIdWait;
begin
  Self.Lock.Acquire;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) then
      Result := NextEvent.TimeToWait
    else
      Result := DefaultSleepTime;
  finally
    Self.Lock.Release;
  end;
end;

procedure TIdTimerQueue.TriggerEarliestEvent;
var
  NextEvent: TIdWait;
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

//******************************************************************************
//* TIdDebugTimerQueue                                                         *
//******************************************************************************
//* TIdDebugTimerQueue Public methods ******************************************

function TIdDebugTimerQueue.EventAt(Index: Integer): TIdWait;
begin
  Result := inherited EventAt(Index);
end;

function TIdDebugTimerQueue.EventCount: Integer;
begin
  Self.LockTimer;
  try
    Result := Self.EventList.Count;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.LockTimer;
begin
  Self.Lock.Acquire;
end;

function TIdDebugTimerQueue.ScheduledEvent(Event: TEvent): Boolean;
begin
  Result := Self.HasScheduledEvent(Event);
end;

function TIdDebugTimerQueue.ScheduledEvent(Event: TNotifyEvent): Boolean;
begin
  Result := Self.HasScheduledEvent(@Event);
end;

procedure TIdDebugTimerQueue.UnlockTimer;
begin
  Self.Lock.Release;
end;

//* TIdDebugTimerQueue Protected methods ***************************************

function TIdDebugTimerQueue.HasScheduledEvent(Event: Pointer): Boolean;
begin
  Result := Self.IndexOfEvent(Event) <> NotFoundSentinelValue;
end;

end.
