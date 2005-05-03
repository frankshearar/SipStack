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
  Classes, Contnrs, IdBaseThread, SyncObjs, SysUtils;

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
    function  MatchEvent(Event: Pointer): Boolean; virtual;
    function  TimeToWait: Cardinal;
    procedure Trigger; virtual; abstract;

    property Data:          TObject  read fData write fData;
    property DebugWaitTime: Cardinal read fDebugWaitTime write fDebugWaitTime;
    property TriggerTime:   Cardinal read fTriggerTime write fTriggerTime;
  end;

  TIdWaitClass = class of TIdWait;

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
  TIdTimerQueue = class(TObject)
  private
    fCreateSuspended: Boolean;
    fEventList:       TList;
    fLock:            TCriticalSection;
    fTerminated:      Boolean;
    fDefaultTimeout:  Cardinal;
    WaitEvent:        TEvent;

    procedure Add(MillisecsWait: Cardinal;
                  Event: TIdWait;
                  Data: TObject);
    procedure ClearEvents;
    function  EarliestEvent: TIdWait;
    function  EventAt(Index: Integer): TIdWait;
    function  GetDefaultTimeout: Cardinal;
    procedure InternalRemove(Event: Pointer);
    procedure SetDefaultTimeout(Value: Cardinal);
    procedure SortEvents;
    function  ShortestWait: Cardinal;
  protected
    function  IndexOfEvent(Event: Pointer): Integer;
    procedure LockTimer; virtual;
    procedure TriggerEarliestEvent; virtual;
    procedure UnlockTimer; virtual;

    property CreateSuspended: Boolean          read fCreateSuspended write fCreateSuspended;
    property EventList:       TList            read fEventList;
    property Lock:            TCriticalSection read fLock;
    property Terminated:      Boolean          read fTerminated write fTerminated;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TEvent;
                       Data: TObject = nil); overload;
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TNotifyEvent;
                       Data: TObject = nil); overload;
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TIdWait); overload; virtual;
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
    procedure RemoveEvent(Event: TEvent); overload;
    procedure RemoveEvent(Event: TNotifyEvent); overload;
    procedure RemoveEvent(Event: TIdWait); overload;
    procedure Resume; virtual;
    procedure Synchronize(Method: TThreadMethod); virtual;
    procedure Terminate; virtual;

    property DefaultTimeout: Cardinal read GetDefaultTimeout write SetDefaultTimeout;
  end;

  TIdThreadProc = procedure of object;

  // The name comes from Smalltalk - a block is a chunk of code, possibly
  // together with some variables. A closure, in other words. Well, this is as
  // close as we get in Delphi.
  TIdBlockRunnerThread = class(TIdBaseThread)
  private
    Block: TIdThreadProc;
  protected
    procedure Run; override;
  public
    constructor Create(Block: TIdThreadProc;
                       CreateSuspended: Boolean = True); reintroduce;
  end;

  TIdTimerEmptyProc = procedure(Sender: TIdTimerQueue) of object;

  // I provide a thread in which to execute my events. Obviously, all
  // TNotifyEvents and such execute in BlockRunner's context.
  TIdThreadedTimerQueue = class(TIdTimerQueue)
  private
    BlockRunner: TIdBlockRunnerThread;
    fOnEmpty:    TIdTimerEmptyProc;

    procedure PossiblyNotifyOfEmpty;
    procedure Run;
  public
    procedure Resume; override;
    procedure Synchronize(Method: TThreadMethod); override;
    procedure Terminate; override;

    property OnEmpty: TIdTimerEmptyProc read fOnEmpty write fOnEmpty;
  end;

  // I provide debugging facilities for you to plug in to things that use
  // TimerQueues.
  TIdDebugTimerQueue = class(TIdTimerQueue)
  private
    fFireImmediateEvents: Boolean;

    function HasScheduledEvent(Event: Pointer): Boolean;
  public
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TIdWait); override;
    function  EventCount: Integer;
    function  FirstEventScheduledFor(Event: Pointer): TIdWait;
    function  LastEventScheduledFor(Event: Pointer): TIdWait;
    procedure LockTimer; override;
    function  ScheduledEvent(Event: TObject): Boolean; overload;
    function  ScheduledEvent(Event: TNotifyEvent): Boolean; overload;
    procedure Terminate; override;
    procedure TriggerAllEventsOfType(WaitType: TIdWaitClass);
    procedure TriggerEarliestEvent; override;
    procedure UnlockTimer; override;

    property FireImmediateEvents: Boolean read fFireImmediateEvents write fFireImmediateEvents;
  end;

const
  DefaultSleepTime   = 1000;
  ItemNotFoundIndex  = -1;
  TriggerImmediately = 0; // zero wait time: execute as soon as possible.

// Math and conversion functions
function AddModulo(Addend, Augend: Cardinal; Radix: Cardinal): Cardinal;
function AddModuloWord(Addend, Augend: Word): Word;

implementation

uses
  IdSystem;

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
//* Unit private functions & procedures                                        *
//******************************************************************************

function TimeSort(Item1, Item2: Pointer): Integer;
var
  WaitA: TIdWait;
  WaitB: TIdWait;
begin
  WaitA := TIdWait(Item1);
  WaitB := TIdWait(Item2);

  if (WaitA.TriggerTime < WaitB.TriggerTime) then
    Result := -1
  else if (WaitA.TriggerTime > WaitB.TriggerTime) then
    Result := 1
  else
    Result := 0;
end;

//******************************************************************************
//* TIdWait                                                                    *
//******************************************************************************
//* TIdWait Public methods *****************************************************

function TIdWait.Due: Boolean;
begin
  Result := GetTickCount >= Self.TriggerTime;
end;

function TIdWait.MatchEvent(Event: Pointer): Boolean;
begin
  Result := Self = Event;
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

constructor TIdTimerQueue.Create(CreateSuspended: Boolean);
begin
  inherited Create;

  Self.fDefaultTimeout := DefaultSleepTime;

  // Before inherited - inherited creates the actual thread and if not
  // suspended will start before we initialize.
  Self.fEventList := TList.Create;
  Self.fLock      := TCriticalSection.Create;
  Self.Terminated := false;
  Self.WaitEvent  := TSimpleEvent.Create;

  Self.CreateSuspended := CreateSuspended;

  if not Self.CreateSuspended then
    Self.Resume;
end;

destructor TIdTimerQueue.Destroy;
begin
  Self.LockTimer;
  try
    Self.WaitEvent.Free;
    Self.ClearEvents;
    Self.EventList.Free;
  finally
    Self.UnlockTimer;
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

procedure TIdTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                 Event: TIdWait);
begin
  Self.Add(MillisecsWait, Event, nil);
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

procedure TIdTimerQueue.RemoveEvent(Event: TIdWait);
begin
  Self.InternalRemove(Event);
end;

procedure TIdTimerQueue.Resume;
begin
end;

procedure TIdTimerQueue.Synchronize(Method: TThreadMethod);
begin
  Method;
end;

procedure TIdTimerQueue.Terminate;
begin
  Self.LockTimer;
  try
    Self.Terminated := true;
    Self.WaitEvent.SetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

//* TIdTimerQueue Protected methods ********************************************

function TIdTimerQueue.IndexOfEvent(Event: Pointer): Integer;
var
  Found: Boolean;
begin
  Found := false;
  Self.LockTimer;
  try
    Result := 0;
    while (Result < Self.EventList.Count) and not Found do begin
      if Self.EventAt(Result).MatchEvent(Event) then
        Found := true
      else
        Inc(Result);
    end;

    if not Found or (Result >= Self.EventList.Count) then
      Result := ItemNotFoundIndex;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.LockTimer;
begin
  Self.Lock.Acquire;
end;

procedure TIdTimerQueue.TriggerEarliestEvent;
var
  FireEvent: Boolean;
  NextEvent: TIdWait;
begin
  FireEvent := false;

  Self.LockTimer;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) and NextEvent.Due then begin
      FireEvent := true;
      Self.EventList.Remove(NextEvent);
    end;

    // Let the worker thread go back to sleep.
    Self.WaitEvent.ResetEvent;
  finally
    Self.UnlockTimer;
  end;

  if FireEvent and not Self.Terminated then begin
    try
      NextEvent.Trigger;
    finally
      NextEvent.Free;
    end;
  end;
end;

procedure TIdTimerQueue.UnlockTimer;
begin
  Self.Lock.Release;
end;

//* TIdTimerQueue Private methods **********************************************

procedure TIdTimerQueue.Add(MillisecsWait: Cardinal;
                            Event: TIdWait;
                            Data: TObject);
begin
  Self.LockTimer;
  try
    try
      Self.EventList.Add(Event);
      Event.Data          := Data;
      Event.DebugWaitTime := MillisecsWait;
      Event.TriggerTime   := AddModulo(GetTickCount,
                                       MillisecsWait,
                                       High(MillisecsWait));
      Self.SortEvents;
    except
      if (Self.EventList.IndexOf(Event) <> ItemNotFoundIndex) then
        Self.EventList.Remove(Event)
      else
        Event.Free;

      raise;
    end;

    Self.WaitEvent.SetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.ClearEvents;
begin
  while Self.EventList.Count > 0 do begin
    Self.EventAt(0).Free;
    Self.EventList.Delete(0);
  end;
end;

function TIdTimerQueue.EarliestEvent: TIdWait;
begin
  // Precondition: Something acquired Self.Lock
  if (Self.EventList.Count = 0) then begin
    Result := nil;
    Exit;
  end;

  Result := Self.EventAt(0);
end;

function TIdTimerQueue.EventAt(Index: Integer): TIdWait;
begin
  // Precondition: Something acquired Self.Lock
  Result := TIdWait(Self.EventList[Index]);
end;

function TIdTimerQueue.GetDefaultTimeout: Cardinal;
begin
  Self.LockTimer;
  try
    Result := Self.fDefaultTimeout;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.InternalRemove(Event: Pointer);
var
  I: Integer;
begin
  // This removes ALL matching wait events matching Event.
  Self.LockTimer;
  try
    I := 0;
    while (I < Self.EventList.Count) do
      if Self.EventAt(I).MatchEvent(Event) then
          Self.EventList.Delete(I)
        else
          Inc(I);

    Self.WaitEvent.SetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.SetDefaultTimeout(Value: Cardinal);
begin
  Self.LockTimer;
  try
    Self.fDefaultTimeout := Value;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.SortEvents;
begin
  // Precondition: You've locked the list
  Self.EventList.Sort(TimeSort);
end;

function TIdTimerQueue.ShortestWait: Cardinal;
var
  NextEvent: TIdWait;
begin
  Self.LockTimer;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) then
      Result := NextEvent.TimeToWait
    else
      Result := Self.DefaultTimeout;
  finally
    Self.UnlockTimer;
  end;
end;

//******************************************************************************
//* TIdBlockRunnerThread                                                       *
//******************************************************************************
//* TIdBlockRunnerThread Public methods ****************************************

constructor TIdBlockRunnerThread.Create(Block: TIdThreadProc;
                                        CreateSuspended: Boolean = True);
begin
  Self.Block := Block;
  Self.FreeOnTerminate := true;

  inherited Create(CreateSuspended);
end;

//* TIdBlockRunnerThread Protected methods *************************************

procedure TIdBlockRunnerThread.Run;
begin
  if Assigned(Self.Block) then
    Self.Block;
end;

//******************************************************************************
//* TIdThreadedTimerQueue                                                      *
//******************************************************************************
//* TIdThreadedTimerQueue Public methods ***************************************

procedure TIdThreadedTimerQueue.Resume;
begin
  inherited Resume;

  if not Assigned(Self.BlockRunner) then
    Self.BlockRunner := TIdBlockRunnerThread.Create(Self.Run, Self.CreateSuspended);

  Self.BlockRunner.Resume;
end;

procedure TIdThreadedTimerQueue.Synchronize(Method: TThreadMethod);
begin
  Self.BlockRunner.Synchronize(Method);
end;

procedure TIdThreadedTimerQueue.Terminate;
begin
  inherited Terminate;

  // This just provides a spot for a bit of documentation:
  // When you call Self.Terminate you will terminate the BlockRunner (since it
  // references Self.Terminated), thus you don't (mustn't!) call
  // Self.BlockRunner.Terminate here.
end;

//* TIdThreadedTimerQueue Private methods **************************************

procedure TIdThreadedTimerQueue.PossiblyNotifyOfEmpty;
begin
  Self.LockTimer;
  try
    if (Self.EventList.Count = 0) and Assigned(Self.fOnEmpty) then
      Self.fOnEmpty(Self);
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdThreadedTimerQueue.Run;
begin
  while not Self.Terminated do begin
    Self.WaitEvent.WaitFor(Self.ShortestWait);

    if not Self.Terminated then
      Self.TriggerEarliestEvent;

    Self.PossiblyNotifyOfEmpty;
  end;
end;

//******************************************************************************
//* TIdDebugTimerQueue                                                         *
//******************************************************************************
//* TIdDebugTimerQueue Public methods ******************************************

procedure TIdDebugTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                      Event: TIdWait);
begin
  if Self.FireImmediateEvents
    and (MillisecsWait = TriggerImmediately) then begin
    Event.Trigger;
    Event.Free;
  end
  else begin
    inherited AddEvent(MillisecsWait, Event);
  end;
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

function TIdDebugTimerQueue.FirstEventScheduledFor(Event: Pointer): TIdWait;
var
  I: Integer;
begin
  Result := nil;

  Self.LockTimer;
  try
    I := 0;
    while (I < Self.EventList.Count) and not Assigned(Result) do begin
      if Self.EventAt(I).MatchEvent(Event) then
        Result := Self.EventAt(I);
      Inc(I);
    end;
  finally
    Self.UnlockTimer;
  end;
end;

function TIdDebugTimerQueue.LastEventScheduledFor(Event: Pointer): TIdWait;
var
  I: Integer;
begin
  Result := nil;

  Self.LockTimer;
  try
    I := Self.EventList.Count - 1;
    while (I >= 0) and not Assigned(Result) do begin
      if Self.EventAt(I).MatchEvent(Event) then
        Result := Self.EventAt(I);
      Dec(I);
    end;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.LockTimer;
begin
  // Expose for debugging purposes.

  inherited LockTimer;
end;

function TIdDebugTimerQueue.ScheduledEvent(Event: TObject): Boolean;
begin
  Result := Self.HasScheduledEvent(Event);
end;

function TIdDebugTimerQueue.ScheduledEvent(Event: TNotifyEvent): Boolean;
begin
  Result := Self.HasScheduledEvent(@Event);
end;

procedure TIdDebugTimerQueue.Terminate;
begin
  inherited Terminate;

  Self.Free;
end;

procedure TIdDebugTimerQueue.TriggerAllEventsOfType(WaitType: TIdWaitClass);
var
  NextEvent: TIdWait;
begin
  Self.LockTimer;
  try
    NextEvent := Self.EarliestEvent;
    while Assigned(NextEvent) do begin
      if (NextEvent is WaitType) then
        NextEvent.Trigger;

      Self.EventList.Remove(NextEvent);
      NextEvent := Self.EarliestEvent;
    end;

    Self.WaitEvent.ResetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.TriggerEarliestEvent;
var
  NextEvent: TIdWait;
begin
  // We fire the next event regardless of whether it's due or not.

  Self.LockTimer;
  try
    NextEvent := Self.EarliestEvent;
    if Assigned(NextEvent) then begin
      NextEvent.Trigger;

      Self.EventList.Remove(NextEvent);
    end;

    Self.WaitEvent.ResetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.UnlockTimer;
begin
  // Expose for debugging purposes.

  inherited UnlockTimer;
end;

//* TIdDebugTimerQueue Protected methods ***************************************

function TIdDebugTimerQueue.HasScheduledEvent(Event: Pointer): Boolean;
begin
  Result := Self.IndexOfEvent(Event) <> ItemNotFoundIndex;
end;

end.
