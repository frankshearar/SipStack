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
  Classes, Contnrs, IdBaseThread, IdNotification, IdRegisteredObject,
  PluggableLogging, SyncObjs, SysUtils;

type
  TIdTimerQueue = class;
  TIdTimerQueueClass = class of TIdTimerQueue;

  TIdLogProc = procedure(Severity: TSeverityLevel;
                         SourceDescription: String;
                         RefCode: Cardinal;
                         Description,
                         BinaryData: String) of object;

  // I represent something that will happen in the future. If you want an alarm
  // to go off in 10 seconds, you'd instantiate me (well, a subclass of me) with
  // a TriggerTime of (Now + 10000) (milliseconds). Then my Trigger will run at
  // the time offset you specify.
  // TimeToWait tells you how soon (in milliseconds) my timer expires.
  //
  // My DebugWaitTime property aids debugging by giving the wait time specified -
  // in our example, a TimerQueue would set DebugWaitTime to 10000, and the
  // TriggerTime to (Now + 10000).
  TIdWait = class(TIdRegisteredObject)
  private
    fDebugWaitTime: Cardinal;
    fOnLog:         TIdLogProc;
    fTriggerTime:   TDateTime;
  protected
    procedure LogTrigger; virtual;
  public
    function  Copy: TIdWait; virtual;
    function  Due: Boolean;
    function  MatchEvent(Event: Pointer): Boolean; virtual;
    procedure Schedule(Delay: Cardinal);
    function  TimeToWait: Cardinal;
    procedure Trigger; virtual;

    property DebugWaitTime: Cardinal   read fDebugWaitTime write fDebugWaitTime;
    property OnLog:         TIdLogProc read fOnLog write fOnLog;
    property TriggerTime:   TDateTime  read fTriggerTime write fTriggerTime;
  end;

  TIdWaitClass = class of TIdWait;

  IIdTimerQueueListener = interface(IInterface)
    ['{29C82AC3-EA30-420D-9073-FA4D25EB7A47}']
    procedure OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
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
  //
  // Note that if you add several Waits with the same MillisecsWait in a very
  // short space of time, I do not guarantee the execution order of the Waits.
  // This is a limitation of using TDateTimes for the Waits' TriggerTimes: the
  // Waits will have TriggerTimes so close together that they're effectively
  // equal.
  TIdTimerQueue = class(TObject)
  private
    fCreateSuspended: Boolean;
    fEventList:       TObjectList;
    fLock:            TCriticalSection;
    fLogName:         String;
    fTerminated:      Boolean;
    fDefaultTimeout:  Cardinal;
    Listeners:        TIdNotificationList;
    WaitEvent:        TEvent;

    procedure ClearEvents;
    function  EarliestEvent: TIdWait;
    function  GetDefaultTimeout: Cardinal;
    procedure NotifyOfException(OffendingWait: TIdWait; Error: Exception);
    procedure SetDefaultTimeout(Value: Cardinal);
//    procedure SortEvents;
    function  ShortestWait: Cardinal;
  protected
    function  EventAt(Index: Integer): TIdWait; virtual;
    function  IndexOfEvent(Event: Pointer): Integer;
    procedure LockTimer; virtual;
    procedure LogTrigger(Severity: TSeverityLevel;
                         SourceDescription: String;
                         RefCode: Cardinal;
                         Description,
                         BinaryData: String);
    procedure TriggerEarliestEvent; virtual;
    procedure TriggerEvent(Event: TIdWait);
    procedure UnlockTimer; virtual;

    property CreateSuspended: Boolean          read fCreateSuspended write fCreateSuspended;
    property EventList:       TObjectList      read fEventList;
    property Lock:            TCriticalSection read fLock;
    property Terminated:      Boolean          read fTerminated write fTerminated;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TIdWait); virtual;
    procedure AddListener(Listener: IIdTimerQueueListener);
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
    procedure RemoveListener(Listener: IIdTimerQueueListener);
    procedure Resume; virtual;
    procedure Terminate; virtual;

    property DefaultTimeout: Cardinal read GetDefaultTimeout write SetDefaultTimeout;
    property LogName:        String   read fLogName write fLogName;
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
  // TIdWaits execute in BlockRunner's context.
  TIdThreadedTimerQueue = class(TIdTimerQueue)
  private
    BlockRunner: TIdBlockRunnerThread;
    fOnEmpty:    TIdTimerEmptyProc;

    procedure PossiblyNotifyOfEmpty;
    procedure Run;
  public
    procedure Resume; override;
    procedure Terminate; override;

    property OnEmpty: TIdTimerEmptyProc read fOnEmpty write fOnEmpty;
  end;

  // I provide debugging facilities for you to plug in to things that use
  // TimerQueues.
  TIdDebugTimerQueue = class(TIdTimerQueue)
  private
    fTriggerImmediateEvents: Boolean;
  public
    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TIdWait); override;
    function  EventAt(Index: Integer): TIdWait; override;
    function  EventCount: Integer;
    function  EventCountFor(WaitType: TIdWaitClass; CountSubclasses: Boolean = false): Integer;
    function  FirstEventScheduledFor(Event: Pointer): TIdWait;
    function  LastEventScheduled: TIdWait; overload;
    function  LastEventScheduled(WaitType: TIdWaitClass): TIdWait; overload;
    function  LastEventScheduledFor(Event: Pointer): TIdWait;
    procedure LockTimer; override;
    procedure RemoveAllEvents;
    function  ScheduledEvent(Event: TIdWait): Boolean; overload;
    function  SecondLastEventScheduled: TIdWait;
    procedure Terminate; override;
    procedure TriggerAllEventsOfType(WaitType: TIdWaitClass);
    procedure TriggerAllEventsUpToFirst(WaitType: TIdWaitClass);
    procedure TriggerEarliestEvent; override;
    procedure UnlockTimer; override;

    property TriggerImmediateEvents: Boolean read fTriggerImmediateEvents write fTriggerImmediateEvents;
  end;

  TIdOnExceptionMethod = class(TIdNotification)
  private
    fError:       Exception;
    fErrorSource: TIdWait;
    fContext:     TIdTimerQueue;
  public
    procedure Run(const Subject: IInterface); override;

    property Error:       Exception     read fError write fError;
    property ErrorSource: TIdWait       read fErrorSource  write fErrorSource;
    property Context:     TIdTimerQueue read fContext write fContext;
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
  DateUtils, IdSystem;

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

function TIdWait.Copy: TIdWait;
begin
  Result := TIdWaitClass(Self.ClassType).Create;
  Result.DebugWaitTime := Self.DebugWaitTime;
  Result.TriggerTime   := Self.TriggerTime;
end;

function TIdWait.Due: Boolean;
begin
  Result := Now >= Self.TriggerTime;
end;

function TIdWait.MatchEvent(Event: Pointer): Boolean;
begin
  Result := Self = TObject(Event);
end;

procedure TIdWait.Schedule(Delay: Cardinal);
begin
  Self.DebugWaitTime := Delay;
  Self.TriggerTime   := Now + OneMillisecond*Delay;
end;

function TIdWait.TimeToWait: Cardinal;
begin
  if Self.Due then
    Result := 0
  else
    Result := MilliSecondsBetween(Now, Self.TriggerTime);
end;

procedure TIdWait.Trigger;
begin
  // By default, do nothing.
  if Assigned(Self.fOnLog) then
    Self.LogTrigger;
end;

//* TIdWait Protected methods **************************************************

procedure TIdWait.LogTrigger;
begin
  // Log anything interesting here. By default, log nothing.
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
  Self.fEventList := TObjectList.Create(false);
  Self.fLock      := TCriticalSection.Create;
  Self.Listeners  := TIdNotificationList.Create;
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
    Self.Listeners.Free;
    Self.ClearEvents;
    Self.EventList.Free;
  finally
    Self.UnlockTimer;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                 Event: TIdWait);
var
  InsertPosition, LookAhead: Integer;
begin
  Event.OnLog := Self.LogTrigger;

  Self.LockTimer;
  try
    try
      {GG: I had to change this algorithm because it does not respect the order of
       media where all the media has the same timestamp}
{      Self.EventList.Add(Event);
      Event.Schedule(Self, MillisecsWait);

      Self.SortEvents;}

      //The new code must schedule the event first so that we have a valid Triggertime value
      //We maintain the list in sorted order by always inserting new events in the appropriate
      //place, after the last event with the same or earlier time - thus creating a fifo sequence
      //for all events with the same triggertime
      Event.Schedule(MillisecsWait);
      //Next, decide on what method to use to find the right insert position in the queue
      case Self.EventList.Count of
        //if the queue is empty, just add the event
        0: Self.EventList.Add(Event);
        //If the queue only contains a few items, we find the appropriate position
        //by a simple backwards scanning algorithm
        1..7: begin
            InsertPosition:=Self.EventList.Count;
            repeat
              if Self.EventAt(InsertPosition-1).TriggerTime<=Event.TriggerTime then
                 Break;
              dec(InsertPosition);
            until InsertPosition=0;
            Self.EventList.Insert(InsertPosition, Event);
          end;
      else
        //otherwise we use a slightly more intelligent approach by using a lookahead algorithm that
        //jumps larger blocks of items, so that even with a few thousand items in the queue,
        //it only takes 10-15 iterations to find the appropriate insert position
        //This is a forward scan, it needs to look for the first event with a later triggertime.
        InsertPosition:=0; //test first item first
        LookAhead:=Self.EventList.Count-1;  //then last item
        repeat
          if Self.EventAt(InsertPosition).TriggerTime>Event.TriggerTime then
             Break;
          if (LookAhead>0) and (Self.EventAt(InsertPosition+LookAhead).TriggerTime<=Event.TriggerTime) then
             InsertPosition:=InsertPosition+LookAhead
          else
             inc(InsertPosition);
          LookAhead:=(Self.EventList.Count-InsertPosition) div 2;
        until InsertPosition>=Self.EventList.Count;
        if InsertPosition>=Self.EventList.Count then
           Self.EventList.Add(Event)
        else
           Self.EventList.Insert(InsertPosition, Event);
      end;

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

procedure TIdTimerQueue.AddListener(Listener: IIdTimerQueueListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdTimerQueue.Before(TimeA,
                              TimeB: Cardinal): Boolean;
begin
  // You can't use Before() to determine the ordering of ticks
  // if the ticks are more than a day apart (i.e., MSecsPerDay ticks).
  Result := GetTickDiff(TimeA, TimeB) < MSecsPerDay;
end;

procedure TIdTimerQueue.RemoveListener(Listener: IIdTimerQueueListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdTimerQueue.Resume;
begin
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

function TIdTimerQueue.EventAt(Index: Integer): TIdWait;
begin
  // Precondition: Something acquired Self.Lock
  Result := TIdWait(Self.EventList[Index]);
end;

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

procedure TIdTimerQueue.LogTrigger(Severity: TSeverityLevel;
                                   SourceDescription: String;
                                   RefCode: Cardinal;
                                   Description,
                                   BinaryData: String);
begin
  LogEntry(Description, SourceDescription, Severity, RefCode, BinaryData);
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

  if FireEvent and not Self.Terminated then
    Self.TriggerEvent(NextEvent);
end;

procedure TIdTimerQueue.TriggerEvent(Event: TIdWait);
begin
  try
    Event.Trigger;
  except
    on E: Exception do
      Self.NotifyOfException(Event, E);
  end;
  Self.EventList.Remove(Event);
  Event.Free;
end;

procedure TIdTimerQueue.UnlockTimer;
begin
  Self.Lock.Release;
end;

//* TIdTimerQueue Private methods **********************************************

procedure TIdTimerQueue.ClearEvents;
begin
  Self.LockTimer;
  try
    while (Self.EventList.Count > 0) do begin
      Self.EventAt(0).Free;
      Self.EventList.Delete(0);
    end;
  finally
    Self.UnlockTimer;
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

function TIdTimerQueue.GetDefaultTimeout: Cardinal;
begin
  Self.LockTimer;
  try
    Result := Self.fDefaultTimeout;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdTimerQueue.NotifyOfException(OffendingWait: TIdWait; Error: Exception);
var
  Notification: TIdOnExceptionMethod;
begin
  Notification := TIdOnExceptionMethod.Create;
  try
    Notification.Error       := Error;
    Notification.ErrorSource := OffendingWait;
    Notification.Context     := Self;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
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

{procedure TIdTimerQueue.SortEvents;
begin
  // Precondition: You've locked the list
  Self.EventList.Sort(TimeSort);
end;}

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
  try
    while not Self.Terminated do begin
      Self.WaitEvent.WaitFor(Self.ShortestWait);

      if not Self.Terminated then
        Self.TriggerEarliestEvent;

      Self.PossiblyNotifyOfEmpty;
    end;
  finally
    Self.Free;
  end;
end;

//******************************************************************************
//* TIdDebugTimerQueue                                                         *
//******************************************************************************
//* TIdDebugTimerQueue Public methods ******************************************

procedure TIdDebugTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                      Event: TIdWait);
begin
  if Self.TriggerImmediateEvents
    and (MillisecsWait = TriggerImmediately) then begin
    Self.TriggerEvent(Event);
  end
  else begin
    inherited AddEvent(MillisecsWait, Event);
  end;
end;

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

function TIdDebugTimerQueue.EventCountFor(WaitType: TIdWaitClass; CountSubclasses: Boolean = false): Integer;
var
  I: Integer;
begin
  Result := 0;

  Self.LockTimer;
  try
    for I := 0 to Self.EventList.Count - 1 do
      if CountSubclasses then begin
        if (Self.EventAt(I) is WaitType) then
          Inc(Result);
      end
      else begin
        if (Self.EventAt(I).ClassName = WaitType.ClassName) then
          Inc(Result);
      end;
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

function TIdDebugTimerQueue.LastEventScheduled: TIdWait;
begin
  Self.LockTimer;
  try
    Result := Self.EventAt(Self.EventCount - 1);
  finally
    Self.UnlockTimer;
  end;
end;

function TIdDebugTimerQueue.LastEventScheduled(WaitType: TIdWaitClass): TIdWait;
var
  I: Integer;
begin
  Result := nil;

  Self.LockTimer;
  try
    I := Self.EventList.Count - 1;
    while (I >= 0) and not Assigned(Result) do begin
      if (Self.EventAt(I).ClassType = WaitType) then
        Result := Self.EventAt(I);
      Dec(I);
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

procedure TIdDebugTimerQueue.RemoveAllEvents;
begin
  Self.ClearEvents;
end;

function TIdDebugTimerQueue.ScheduledEvent(Event: TIdWait): Boolean;
begin
  Result := Self.IndexOfEvent(Event) <> ItemNotFoundIndex;
end;

function TIdDebugTimerQueue.SecondLastEventScheduled: TIdWait;
begin
  Self.LockTimer;
  try
    if (Self.EventCount < 2) then begin
      Result := nil;
      Exit;
    end;

    Result := Self.EventAt(Self.EventCount - 2);
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.Terminate;
begin
  inherited Terminate;

  Self.Free;
end;

procedure TIdDebugTimerQueue.TriggerAllEventsOfType(WaitType: TIdWaitClass);
var
  NextEvent: TIdWait;
  I:         Integer;
begin
  Self.LockTimer;
  try
    I := 0;
    while (I < Self.EventList.Count) do begin
      NextEvent := Self.EventAt(I);

      if (NextEvent is WaitType) then
        Self.TriggerEvent(NextEvent)
      else
        Inc(I);
    end;

    Self.WaitEvent.ResetEvent;
  finally
    Self.UnlockTimer;
  end;
end;

procedure TIdDebugTimerQueue.TriggerAllEventsUpToFirst(WaitType: TIdWaitClass);
var
  Done:      Boolean;
  NextEvent: TIdWait;
begin
  Self.LockTimer;
  try
    Done := false;
    NextEvent := Self.EarliestEvent;
    while Assigned(NextEvent) and not Done do begin
      Done := NextEvent is WaitType;
      Self.TriggerEvent(NextEvent);

      NextEvent := Self.EarliestEvent;
    end;
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
    if Assigned(NextEvent) then
      Self.TriggerEvent(NextEvent);

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

//******************************************************************************
//* TIdOnExceptionMethod                                                       *
//******************************************************************************
//* TIdOnExceptionMethod Public methods ****************************************

procedure TIdOnExceptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdTimerQueueListener).OnException(Self.Context,
                                                 Self.Error,
                                                 Self.ErrorSource);
end;

end.
