{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdTimerQueue;

interface

uses
  Classes, IdInterfacedObject, IdTimerQueue, SyncObjs, SysUtils, TestFramework,
  TestFrameworkEx, TestFrameworkTimerQueue;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestAddModulo;
    procedure TestAddModuloWord;
  end;

  // I wait in a separate thread for something to set Event. Then I
  // fire off the callback OnEventSet and terminate.
  TThreadEvent = class(TThread)
  private
    Event:      TEvent;
    MaxWait:    Cardinal;
    OnEventSet: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Event: TEvent;
                       OnEventSet: TNotifyEvent;
                       MaxWait: Cardinal);
  end;

  TestTIdWait = class(TTestCase)
  private
    Wait: TIdWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdTimerQueue = class(TTestCase)
  private
    Queue:    TIdDebugTimerQueue;
    WaitTime: Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEvent;
    procedure TestBefore;
  end;

  TestTIdThreadedTimerQueue = class(TThreadingTestCase,
                                    IIdTimerQueueListener)
  private
    CallbackEventOne: TEvent;
    CallbackEventTwo: TEvent;
    Data:             TObject;
    EventOne:         TEvent;
    EventTwo:         TEvent;
    Lock:             TCriticalSection;
    LogOutput:        String;
    Notified:         Boolean;
    OrderOfFire:      String;
    Queue:            TIdThreadedTimerQueue;
    T1:               TThreadEvent;
    T2:               TThreadEvent;

    procedure OnEventOneSet(Sender: TObject);
    procedure OnEventTwoSet(Sender: TObject);
    procedure OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
    procedure WaitForAll(Events: array of TEvent;
                         Timeout: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOneEvent;
    procedure TestResume;
    procedure TestTwoEvents;
    procedure TestTwoOutOfOrderEvents;
    procedure TestWaitForEarliestEvent;
  end;

  TestTIdDebugTimerQueue = class(TTestCase)
  private
    OnSecondTimerFired: Boolean;
    OnTimerFired:       Boolean;
    Timer:              TIdDebugTimerQueue;
    WaitEvent:          TEvent;

    procedure OnSecondTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEventWithZeroTimeToSuspendedTimer;
    procedure TestCount;
    procedure TestDebugWaitTime;
    procedure TestExceptionNotifiesListeners;
    procedure TestRemoveAllEvents;
    procedure TestScheduledEvent;
    procedure TestTriggerEarliestEvent;
  end;

  TTimerQueueListener = class(TIdInterfacedObject,
                              IIdTimerQueueListener)
  private
    fContextParam:     TIdTimerQueue;
    fErrorParam:       Exception;
    fExceptionRaised:  Boolean;
    fErrorSourceParam: TIdWait;

    procedure OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
  public
    constructor Create; override;

    property ContextParam:     TIdTimerQueue  read fContextParam;
    property ErrorParam:       Exception      read fErrorParam;
    property ExceptionRaised:  Boolean        read fExceptionRaised;
    property ErrorSourceParam: TIdWait        read fErrorSourceParam;
  end;

  TestTIdOnExceptionMethod = class(TTestCase)
  private
    Context:     TIdTimerQueue;
    Error:       Exception;
    ErrorSource: TIdWait;
    Listener:    TTimerQueueListener;
    Method:      TIdOnExceptionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

const
  ShortTimeout = 10;              // milliseconds
  LongTimeout  = 10*ShortTimeout; // milliseconds

implementation

uses
  IdGlobal;

type
  TIdTestWait = class(TIdWait)
  private
    fTriggered: Boolean;
  public
    procedure Trigger; override;

    property Triggered: Boolean read fTriggered;
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

  TIdNullWait = class(TIdWait)
  public
    procedure Trigger; override;
  end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdTimerQueue unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdWait.Suite);
  Result.AddTest(TestTIdTimerQueue.Suite);
  Result.AddTest(TestTIdThreadedTimerQueue.Suite);
  Result.AddTest(TestTIdDebugTimerQueue.Suite);
  Result.AddTest(TestTIdOnExceptionMethod.Suite);
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
//* TIdTestWait                                                                *
//******************************************************************************
//* TIdTestWait Public methods *************************************************

procedure TIdTestWait.Trigger;
begin
  Self.fTriggered := true;
end;

//******************************************************************************
//* TIdNullWait                                                                *
//******************************************************************************
//* TIdNullWait Public methods *************************************************

procedure TIdNullWait.Trigger;
begin
  // Do nothing.
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestAddModulo;
begin
  CheckEquals(8, AddModulo(7, 1, 10),   'AddModulo(7, 1, 10)');
  CheckEquals(8, AddModulo(7, 11, 10),  'AddModulo(7, 11, 10)');
  CheckEquals(8, AddModulo(7, 101, 10), 'AddModulo(7, 101, 10)');
  CheckEquals(0, AddModulo(7, 1, 8),    'AddModulo(7, 1, 8)');

  CheckEquals(0,
              AddModulo($fffffffe, 1, $ffffffff),
              'AddModulo($fffffffe, 1, $ffffffff)');

  CheckEquals(1,
              AddModulo($ffffffff, 1, $ffffffff),
              'AddModulo($ffffffff, 1, $ffffffff)');
end;

procedure TestFunctions.TestAddModuloWord;
begin
  CheckEquals(8,  AddModuloWord(7, 1),   'AddModuloWord(7, 1)');
  CheckEquals(18, AddModuloWord(7, 11),  'AddModuloWord(7, 11)');

  CheckEquals(0,
              AddModuloWord($fffe, 1),
              'AddModuloWord($fffe, 1)');

  CheckEquals(1,
              AddModuloWord($ffff, 1),
              'AddModuloWord($ffff, 1)');
end;

//******************************************************************************
//* TThreadEvent                                                               *
//******************************************************************************
//* TThreadEvent Public methods ************************************************

constructor TThreadEvent.Create(Event: TEvent;
                                OnEventSet: TNotifyEvent;
                                MaxWait: Cardinal);
begin
  inherited Create(true);

  Self.Event      := Event;
  Self.MaxWait    := MaxWait;
  Self.OnEventSet := OnEventSet;

//  Self.FreeOnTerminate := true;
end;

//* TThreadEvent Protected methods *********************************************

procedure TThreadEvent.Execute;
begin
  Self.Event.WaitFor(Self.MaxWait);

  if not Self.Terminated and Assigned(Self.OnEventSet) then
    Self.OnEventSet(Self);
end;

//******************************************************************************
//* TestTIdWait                                                                *
//******************************************************************************
//* TestTIdWait Public methods *************************************************

procedure TestTIdWait.SetUp;
begin
  inherited SetUp;

  Self.Wait := TIdNullWait.Create;
  Self.Wait.DebugWaitTime := $decafbad;
  Self.Wait.TriggerTime   := Now;
end;

procedure TestTIdWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdWait Published methods **********************************************

procedure TestTIdWait.TestCopy;
var
  Other: TIdWait;
begin
  Other := Self.Wait.Copy;
  try
    CheckEquals(IntToHex(Self.Wait.DebugWaitTime, 8),
                IntToHex(Other.DebugWaitTime, 8),
                'DebugWaitTime');
    CheckEquals(FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Self.Wait.TriggerTime),
                FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Other.TriggerTime),
                'TriggerTime');
  finally
    Other.Free;
  end;
end;

//******************************************************************************
//* TestTIdTimerQueue                                                          *
//******************************************************************************
//* TestTIdTimerQueue Public methods *******************************************

procedure TestTIdTimerQueue.SetUp;
begin
  inherited SetUp;

  Self.Queue := TIdDebugTimerQueue.Create(true);

  Self.WaitTime := 999;
end;

procedure TestTIdTimerQueue.TearDown;
begin
  Self.Queue.Terminate;

  inherited TearDown;
end;

//* TestTIdTimerQueue Published methods ****************************************

procedure TestTIdTimerQueue.TestAddEvent;
var
  Foo: TIdTestWait;
begin
  Foo := TIdTestWait.Create;
  Self.Queue.AddEvent(WaitTime, Foo);
  CheckEquals(1, Self.Queue.EventCount, 'No event added');
  Check(Self.Queue.ScheduledEvent(Foo), 'No event scheduled');
  CheckNotEquals(0, Self.Queue.FirstEventScheduledFor(Foo).TimeToWait, 'TimeToWait not calculated');
end;

procedure TestTIdTimerQueue.TestBefore;
begin
  Check(    Self.Queue.Before(10, 20), '10, 20');
  Check(not Self.Queue.Before(20, 10), '20, 10');

  Check(    Self.Queue.Before(0, 1), '0, 1');
  Check(not Self.Queue.Before(1, 0), '1, 0');
  Check(    Self.Queue.Before(High(Cardinal), 0), 'High(Cardinal), 0');
  Check(not Self.Queue.Before(0, High(Cardinal)), '0, High(Cardinal)');

  Check(Self.Queue.Before(High(Cardinal) - 20, High(Cardinal) - 10),
        'High(Cardinal) - 20, High(Cardinal) - 10');
  Check(not Self.Queue.Before(High(Cardinal) - 10, High(Cardinal) - 20),
        'High(Cardinal) - 10, High(Cardinal) - 20');

  Check(Self.Queue.Before(High(Cardinal) - 10, 0),
        'High(Cardinal) - 10, 0');
  Check(not Self.Queue.Before(0, High(Cardinal) - 10),
        '0, High(Cardinal) - 10');
  Check(Self.Queue.Before(High(Cardinal) - 10, 10),
        'High(Cardinal) - 10, 10');
  Check(not Self.Queue.Before(10, High(Cardinal) - 10),
        '10, High(Cardinal) - 10');
end;

//******************************************************************************
//* TestTIdThreadedTimerQueue                                                  *
//******************************************************************************
//* TestTIdThreadedTimerQueue Public methods ***********************************

procedure TestTIdThreadedTimerQueue.SetUp;
begin
  inherited SetUp;

  Self.DefaultTimeout   := LongTimeout;
  Self.ExceptionMessage := 'The event waited for was never fired';
  Self.LogOutput        := '';
  Self.Notified         := false;
  Self.OrderOfFire      := '';

  Self.CallbackEventOne := TSimpleEvent.Create;
  Self.CallbackEventTwo := TSimpleEvent.Create;
  Self.Data             := TObject.Create;
  Self.EventOne         := TSimpleEvent.Create;
  Self.EventTwo         := TSimpleEvent.Create;
  Self.Lock             := TCriticalSection.Create;
  Self.Queue            := TIdThreadedTimerQueue.Create(false);

  // You set Self.EventOne. That makes Self.T1 wake up and fire
  // Self.OnEventOneSet which sets Self.CallbackEventOne. We do this complicated
  // mission so that Self.OnEventOneSet doesn't run from within the main (VCL/CLX)
  // thread, allowing us to capture the order in which events fire.

  Self.T1 := TThreadEvent.Create(Self.EventOne,
                                 Self.OnEventOneSet,
                                 LongTimeout);
  Self.T2 := TThreadEvent.Create(Self.EventTwo,
                                 Self.OnEventTwoSet,
                                 LongTimeout);

  Self.T1.Resume;
  Self.T2.Resume;
end;

procedure TestTIdThreadedTimerQueue.TearDown;
begin
  Self.T2.Terminate;
  Self.T2.WaitFor;
  Self.T2.Free;
  Self.T1.Terminate;
  Self.T1.WaitFor;
  Self.T1.Free;

  Self.Queue.Terminate;
  Self.Lock.Free;
  Self.EventTwo.Free;
  Self.EventOne.Free;
  Self.Data.Free;
  Self.CallbackEventTwo.Free;
  Self.CallbackEventOne.Free;

  inherited TearDown;
end;

//* TestTIdThreadedTimerQueue Private methods **********************************

procedure TestTIdThreadedTimerQueue.OnEventOneSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '1';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventOne.SetEvent;
end;

procedure TestTIdThreadedTimerQueue.OnEventTwoSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '2';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventTwo.SetEvent;
end;

procedure TestTIdThreadedTimerQueue.OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
begin
  Self.ExceptionMessage := Error.Message;
  Self.ExceptionType    := ExceptClass(Error.ClassType);

  Self.EventTwo.SetEvent;
end;

procedure TestTIdThreadedTimerQueue.WaitForAll(Events: array of TEvent;
                                       Timeout: Cardinal);
var
  I:   Integer;
  Msg: String;
begin
  Msg := Self.ExceptionMessage;
  try
    for I := Low(Events) to High(Events) do begin
      Self.ExceptionMessage := Msg + '; Event ' + IntToStr(I);
      Self.WaitForSignaled(Events[I]);
    end;
  finally
    Self.ExceptionMessage := Msg;
  end;
end;

//* TestTIdThreadedTimerQueue Published methods ********************************

procedure TestTIdThreadedTimerQueue.TestOneEvent;
var
  Wait: TIdEventWait;
begin
  TThreadEvent.Create(Self.EventOne,
                      Self.OnEventOneSet,
                      LongTimeout);

  Wait := TIdEventWait.Create;
  Wait.Event := Self.EventOne;

  Self.Queue.AddEvent(ShortTimeout, Wait);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.EventOne);
end;

procedure TestTIdThreadedTimerQueue.TestResume;
var
  NewTimer: TIdThreadedTimerQueue;
  Wait:     TIdEventWait;
begin
  NewTimer := TIdThreadedTimerQueue.Create(true);
  try
    Wait := TIdEventWait.Create;
    Wait.Event := Self.EventOne;

    NewTimer.AddEvent(ShortTimeout, Wait);

    NewTimer.Resume;

    Self.WaitForSignaled(Self.EventOne, 'Event didn''t fire');
  finally
    NewTimer.Terminate;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestTwoEvents;
var
  WaitOne,
  WaitTwo: TIdEventWait;
begin
  TThreadEvent.Create(Self.EventOne,
                      Self.OnEventOneSet,
                      LongTimeout);
  TThreadEvent.Create(Self.EventTwo,
                      Self.OnEventTwoSet,
                      LongTimeout);

  WaitOne := TIdEventWait.Create;
  WaitOne.Event := Self.EventOne;

  WaitTwo := TIdEventWait.Create;
  WaitTwo.Event := Self.EventTwo;

  Self.Queue.AddEvent(ShortTimeout,   WaitOne);
  Self.Queue.AddEvent(2*ShortTimeout, WaitTwo);

  Self.WaitForAll([Self.CallbackEventOne, Self.CallbackEventTwo],
                  LongTimeout);

  Self.Lock.Acquire;
  try
    CheckEquals('12', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestTwoOutOfOrderEvents;
var
  WaitOne,
  WaitTwo: TIdEventWait;
begin
  WaitOne := TIdEventWait.Create;
  WaitOne.Event := Self.EventOne;

  WaitTwo := TIdEventWait.Create;
  WaitTwo.Event := Self.EventTwo;

  Self.Queue.AddEvent(2*ShortTimeout, WaitOne);
  Self.Queue.AddEvent(ShortTimeout,   WaitTwo);

  Self.WaitForAll([Self.CallbackEventOne, Self.CallbackEventTwo],
                  LongTimeout);

  Self.Lock.Acquire;
  try
    CheckEquals('21', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestWaitForEarliestEvent;
var
  WaitOne,
  WaitTwo: TIdEventWait;
begin
  WaitOne := TIdEventWait.Create;
  WaitOne.Event := Self.EventOne;

  WaitTwo := TIdEventWait.Create;
  WaitTwo.Event := Self.EventTwo;

  Self.Queue.AddEvent(2*ShortTimeout, WaitOne);
  Self.Queue.AddEvent(ShortTimeout,   WaitTwo);

  Self.WaitForSignaled(Self.CallbackEventTwo);

  Self.Lock.Acquire;
  try
    CheckEquals('2', Self.OrderOfFire, 'We expect only EventTwo to fire');
  finally
    Self.Lock.Release;
  end;

  Self.WaitForSignaled(Self.CallbackEventOne);

  Self.Lock.Acquire;
  try
    CheckEquals('21', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

//******************************************************************************
//* TestTIdDebugTimerQueue                                                     *
//******************************************************************************
//* TestTIdDebugTimerQueue Public methods **************************************

procedure TestTIdDebugTimerQueue.SetUp;
begin
  inherited SetUp;

  Self.WaitEvent := TSimpleEvent.Create;
  Self.Timer     := TIdDebugTimerQueue.Create(false);

  Self.OnSecondTimerFired := false;
  Self.OnTimerFired       := false;
end;

procedure TestTIdDebugTimerQueue.TearDown;
begin
  Self.Timer.Terminate;
  Self.WaitEvent.Free;

  inherited TearDown;
end;

//* TestTIdDebugTimerQueue Private methods *************************************

procedure TestTIdDebugTimerQueue.OnSecondTimer(Sender: TObject);
begin
  Self.OnSecondTimerFired := true;
end;

procedure TestTIdDebugTimerQueue.OnTimer(Sender: TObject);
begin
  Self.OnTimerFired := true;
end;

//* TestTIdDebugTimerQueue Published methods ***********************************

procedure TestTIdDebugTimerQueue.TestAddEventWithZeroTimeToSuspendedTimer;
var
  EventCount:   Integer;
  SuspTimer:    TIdDebugTimerQueue;
  ZeroTimeWait: TIdWait;
begin
  SuspTimer := TIdDebugTimerQueue.Create(true);
  try
    ZeroTimeWait := TIdWait.Create;
    EventCount := SuspTimer.EventCount;

    SuspTimer.AddEvent(0, ZeroTimeWait);

    Check(EventCount < SuspTimer.EventCount,
          'SuspTimer executed scheduled event despite not running');
  finally
    SuspTimer.Terminate;
  end;
end;

procedure TestTIdDebugTimerQueue.TestCount;
var
  I:    Integer;
  Wait: TIdNotifyEventWait;
begin
  for I := 1 to 10 do begin
    Wait := TIdNotifyEventWait.Create;
    Wait.Event := Self.OnTimer;
    Self.Timer.AddEvent(1000, Wait);

    CheckEquals(I, Self.Timer.EventCount, IntToStr(I) + 'th AddEvent');
  end;
end;

procedure TestTIdDebugTimerQueue.TestDebugWaitTime;
const
  ArbValue = 42;
var
  Wait: TIdNotifyEventWait;
begin
  Wait := TIdNotifyEventWait.Create;
  Wait.Event := Self.OnTimer;

  Self.Timer.AddEvent(ArbValue, Wait);
  CheckEquals(1, Self.Timer.EventCount, 'Unexpected number of Waits scheduled');

  CheckEquals(ArbValue, Self.Timer.LastEventScheduled.DebugWaitTime,
              'DebugWaitTime not set');
end;

procedure TestTIdDebugTimerQueue.TestExceptionNotifiesListeners;
var
  L1, L2: TTimerQueueListener;
  Raiser: TExceptionRaisingWait;
begin
  Raiser := TExceptionRaisingWait.Create;
  Raiser.ErrorMessage := 'You silly sausage';
  Raiser.ErrorType    := EDivByZero;

  Self.Timer.AddEvent(TriggerImmediately, Raiser);

  L1 := TTimerQueueListener.Create;
  try
    L2 := TTimerQueueListener.Create;
    try
      Self.Timer.AddListener(L1);
      Self.Timer.AddListener(L2);

      Self.Timer.TriggerEarliestEvent;

      Check(L1.ExceptionRaised, 'Listener 1 not notified');
      Check(L2.ExceptionRaised, 'Listener 2 not notified');
    finally
      Self.Timer.RemoveListener(L2);
      L2.Free;
    end;
  finally
    Self.Timer.RemoveListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdDebugTimerQueue.TestRemoveAllEvents;
var
  WaitOne: TIdWait;
  WaitTwo: TIdWait;
begin
  // The time frees these
  WaitOne := TIdWait.Create;
  WaitTwo := TIdWait.Create;
  Self.Timer.AddEvent(1000, WaitOne);
  Self.Timer.AddEvent(1000, WaitTwo);

  Self.Timer.RemoveAllEvents;

  CheckEquals(0, Self.Timer.EventCount, 'Not all events removed');
end;

procedure TestTIdDebugTimerQueue.TestScheduledEvent;
var
  Wait: TIdWait;
begin
  // The timer frees this.
  Wait := TIdWait.Create;

  Check(not Self.Timer.ScheduledEvent(Wait),
        'TIdWait not yet scheduled');

  Self.Timer.AddEvent(1000, Wait);

  Check(Self.Timer.ScheduledEvent(Wait),
        'TEvent not scheduled');
end;

procedure TestTIdDebugTimerQueue.TestTriggerEarliestEvent;
var
  WaitOne,
  WaitTwo: TIdNotifyEventWait;
begin
  WaitOne := TIdNotifyEventWait.Create;
  WaitOne.Event := Self.OnTimer;
  WaitTwo := TIdNotifyEventWait.Create;
  WaitTwo.Event := Self.OnSecondTimer;

  Self.Timer.AddEvent(1000, WaitOne);
  Self.Timer.AddEvent(2000, WaitTwo);

  Self.Timer.TriggerEarliestEvent;

  Check(Self.OnTimerFired, 'First not triggered');
  Check(not Self.OnSecondTimerFired, 'Second event triggered');

  Self.Timer.TriggerEarliestEvent;

  Check(Self.OnSecondTimerFired, 'Second event not triggered');
end;

//******************************************************************************
//* TTimerQueueListener                                                        *
//******************************************************************************
//* TTimerQueueListener Public methods *****************************************

constructor TTimerQueueListener.Create;
begin
  inherited Create;

  Self.fExceptionRaised := false;
end;

//* TTimerQueueListener Protected methods **************************************

procedure TTimerQueueListener.OnException(Timer: TIdTimerQueue; Error: Exception; Wait: TIdWait);
begin
  Self.fContextParam     := Timer;
  Self.fErrorParam       := Error;
  Self.fErrorSourceParam := Wait;
  Self.fExceptionRaised  := true;
end;

//******************************************************************************
//* TestTIdOnExceptionMethod                                                   *
//******************************************************************************
//* TestTIdOnExceptionMethod Public methods ************************************

procedure TestTIdOnExceptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Context     := TIdDebugTimerQueue.Create(false);
  Self.Error       := EAccessViolation.Create('Access Violation Occured Nowhere');
  Self.ErrorSource := TIdTestWait.Create;
  Self.Listener    := TTimerQueueListener.Create;
  Self.Method      := TIdOnExceptionMethod.Create;

  Self.Method.Error       := Self.Error;
  Self.Method.ErrorSource := Self.ErrorSource;
  Self.Method.Context     := Self.Context;
end;

procedure TestTIdOnExceptionMethod.TearDown;
begin
  Self.Method.Free;
  Self.Listener.Free;
  Self.ErrorSource.Free;
  Self.Error.Free;
  Self.Context.Free;

  inherited TearDown;
end;

//* TestTIdOnExceptionMethod Published methods *********************************

procedure TestTIdOnExceptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.ExceptionRaised, 'Listener not notified');

  Check(Self.Error = Self.Listener.ErrorParam, 'Error parameter');
  Check(Self.Context = Self.Listener.ContextParam, 'Context parameter');
  Check(Self.ErrorSource = Self.Listener.ErrorSourceParam,   'ErrorSource parameter');
end;

initialization
  RegisterTest('IdRTPTimerQueue', Suite);
end.
