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
    Data: TObject;
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
    procedure TestRemoveEvent;
  end;

  TestTIdThreadedTimerQueue = class(TThreadingTestCase)
  private
    CallbackEventOne: TEvent;
    CallbackEventTwo: TEvent;
    Data:             TObject;
    EventOne:         TEvent;
    EventTwo:         TEvent;
    Lock:             TCriticalSection;
    Notified:         Boolean;
    OrderOfFire:      String;
    Queue:            TIdThreadedTimerQueue;
    T1:               TThreadEvent;
    T2:               TThreadEvent;

    procedure CheckNotifyEvent(Sender: TObject);
    procedure CheckNotifyEventWithData(Sender: TObject);
    procedure NotifyEventOne(Sender: TObject);
    procedure NotifyEventTwo(Sender: TObject);
    procedure OnEventOneSet(Sender: TObject);
    procedure OnEventTwoSet(Sender: TObject);
    procedure WaitForAll(Events: array of TEvent;
                         Timeout: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNotifyEvent;
    procedure TestNotifyEventWithData;
    procedure TestOneEvent;
    procedure TestRemoveEvent;
    procedure TestRemoveNonExistentEvent;
    procedure TestRemoveNonExistentEventWithOtherEvents;
    procedure TestRemoveNonExistentNotifyEventWithOtherNotifyEvents;
    procedure TestRemoveNotifyEvent;
    procedure TestResume;
    procedure TestTwoEvents;
    procedure TestTwoNotifyEvents;
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
    constructor Create;

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
  TIdFooEvent = class(TIdWait)
  private
    fTriggered: Boolean;
  public
    procedure Trigger; override;

    property Triggered: Boolean read fTriggered;
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
//* TIdFooEvent                                                                *
//******************************************************************************
//* TIdFooEvent Public methods *************************************************

procedure TIdFooEvent.Trigger;
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
  Self.Wait.Data          := TObject.Create;
  Self.Wait.DebugWaitTime := $decafbad;
  Self.Wait.TriggerTime   := Now;
end;

procedure TestTIdWait.TearDown;
begin
  Self.Wait.Data.Free;
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
    Check(Self.Wait.Data = Other.Data, 'Data');
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
  Foo: TIdFooEvent;
begin
  Foo := TIdFooEvent.Create;
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

procedure TestTIdTimerQueue.TestRemoveEvent;
var
  Foo: TIdFooEvent;
begin
  Foo := TIdFooEvent.Create;
  Self.Queue.AddEvent(WaitTime, Foo);
  Self.Queue.RemoveEvent(Foo);
  CheckEquals(0, Self.Queue.EventCount, 'No event removed');
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

procedure TestTIdThreadedTimerQueue.CheckNotifyEvent(Sender: TObject);
begin
  Self.Notified := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdThreadedTimerQueue.CheckNotifyEventWithData(Sender: TObject);
begin
  try
    CheckEquals(Sender.ClassName,
                TIdNotifyEventWait.ClassName,
                'Unexpected object in CheckNotifyEventWithData');

    Check(Self.Data = (Sender as TIdNotifyEventWait).Data,
          'Unexpected data object');
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;

  Self.CheckNotifyEvent(Sender);
end;

procedure TestTIdThreadedTimerQueue.NotifyEventOne(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'a';
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdThreadedTimerQueue.NotifyEventTwo(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'b';
  finally
    Self.Lock.Release;
  end;
  Self.ThreadEvent.SetEvent;
end;

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

procedure TestTIdThreadedTimerQueue.TestNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout, Self.CheckNotifyEvent);

  Self.WaitForSignaled(Self.ThreadEvent, 'Event didn''t fire');
  Check(Self.Notified, 'TNotifyEvent didn''t fire');
end;

procedure TestTIdThreadedTimerQueue.TestNotifyEventWithData;
begin
  Self.Queue.AddEvent(ShortTimeout,
                      Self.CheckNotifyEventWithData,
                      Self.Data);

  Self.WaitForSignaled(Self.ThreadEvent, 'Event didn''t fire');

  Check(Self.Notified, 'TNotifyEvent didn''t fire');
end;

procedure TestTIdThreadedTimerQueue.TestOneEvent;
begin
  TThreadEvent.Create(Self.EventOne,
                      Self.OnEventOneSet,
                      LongTimeout);

  Self.Queue.AddEvent(ShortTimeout, Self.EventOne);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.EventOne);
end;

procedure TestTIdThreadedTimerQueue.TestRemoveEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.EventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventOne);
  Self.Queue.AddEvent(3*ShortTimeout, Self.EventTwo);
  Self.Queue.RemoveEvent(Self.EventOne);

  Self.ExceptionMessage := 'EventTwo didn''t fire';

  Self.WaitForSignaled(Self.EventTwo);
  CheckEquals(0,
              Pos('1', Self.OrderOfFire),
              'EventOne wasn''t removed (Order of fire was '
            + Self.OrderOfFire + ')');
end;

procedure TestTIdThreadedTimerQueue.TestRemoveNonExistentEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.EventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventTwo);

  Self.ExceptionMessage := 'EventTwo didn''t fire';

  Self.Queue.RemoveEvent(Self.EventOne);
  Self.WaitForSignaled(Self.EventTwo);
  CheckEquals(0,
              Pos('1', Self.OrderOfFire),
              'EventOne wasn''t removed (Order of fire was '
            + Self.OrderOfFire + ')');
end;

procedure TestTIdThreadedTimerQueue.TestRemoveNonExistentEventWithOtherEvents;
begin
  // This catches a bug where if there were multiple TEvents
  // and you removed a non-existent one you'd enter an infinite loop.
  // Simple implementation bug.

  Self.Queue.AddEvent(ShortTimeout, Self.EventOne);
  Self.Queue.RemoveEvent(Self.EventTwo);
end;

procedure TestTIdThreadedTimerQueue.TestRemoveNonExistentNotifyEventWithOtherNotifyEvents;
begin
  // This catches a bug where if there were multiple TNotifyEvents
  // and you removed a non-existent one you'd enter an infinite loop.
  // Simple implementation bug.

  Self.Queue.AddEvent(ShortTimeout, Self.NotifyEventOne);
  Self.Queue.RemoveEvent(Self.NotifyEventTwo);
end;

procedure TestTIdThreadedTimerQueue.TestRemoveNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.CheckNotifyEvent);
  Self.Queue.AddEvent(2*ShortTimeout, Self.CheckNotifyEvent);
  Self.Queue.AddEvent(3*ShortTimeout, Self.EventOne);
  Self.Queue.RemoveEvent(Self.CheckNotifyEvent);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.EventOne);
  Check(not Self.Notified, 'TNotifyEvent wasn''t removed');
end;

procedure TestTIdThreadedTimerQueue.TestResume;
var
  NewTimer: TIdThreadedTimerQueue;
begin
  NewTimer := TIdThreadedTimerQueue.Create(true);
  try
    NewTimer.AddEvent(ShortTimeout, Self.EventOne);

    NewTimer.Resume;

    Self.WaitForSignaled(Self.EventOne, 'Event didn''t fire');
  finally
    NewTimer.Terminate;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestTwoEvents;
begin
  TThreadEvent.Create(Self.EventOne,
                      Self.OnEventOneSet,
                      LongTimeout);
  TThreadEvent.Create(Self.EventTwo,
                      Self.OnEventTwoSet,
                      LongTimeout);

  Self.Queue.AddEvent(ShortTimeout,   Self.EventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventTwo);

  Self.WaitForAll([Self.CallbackEventOne, Self.CallbackEventTwo],
                  LongTimeout);

  Self.Lock.Acquire;
  try
    CheckEquals('12', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestTwoNotifyEvents;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.NotifyEventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.NotifyEventTwo);

  Self.WaitForSignaled(Self.ThreadEvent);

  Self.Lock.Acquire;
  try
    CheckEquals('ab', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdThreadedTimerQueue.TestTwoOutOfOrderEvents;
begin
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventOne);
  Self.Queue.AddEvent(ShortTimeout,   Self.EventTwo);

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
begin
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventOne);
  Self.Queue.AddEvent(ShortTimeout,   Self.EventTwo);

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
  Self.OnTimerFired := false;
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
  EventCount: Integer;
  SuspTimer:  TIdDebugTimerQueue;
begin
  SuspTimer := TIdDebugTimerQueue.Create(true);
  try
    EventCount := SuspTimer.EventCount;

    SuspTimer.AddEvent(0, Self.OnTimer, nil);

    Check(EventCount < SuspTimer.EventCount,
          'SuspTimer executed scheduled event despite not running');
  finally
    SuspTimer.Terminate;
  end;
end;

procedure TestTIdDebugTimerQueue.TestCount;
var
  I: Integer;
begin
  for I := 1 to 10 do begin
    Self.Timer.AddEvent(1000, Self.OnTimer, nil);

    CheckEquals(I, Self.Timer.EventCount, IntToStr(I) + 'th AddEvent');
  end;
end;

procedure TestTIdDebugTimerQueue.TestDebugWaitTime;
const
  ArbValue = 42;
var
  Event: TNotifyEvent;
begin
  Event := Self.OnTimer;

  Self.Timer.AddEvent(ArbValue, Event, nil);
  CheckEquals(ArbValue, Self.Timer.FirstEventScheduledFor(@Event).DebugWaitTime,
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
begin
  Self.Timer.AddEvent(1000, Self.WaitEvent);
  Self.Timer.AddEvent(2000, Self.OnTimer);

  Self.Timer.RemoveAllEvents;

  CheckEquals(0, Self.Timer.EventCount, 'Not all events removed');
end;

procedure TestTIdDebugTimerQueue.TestScheduledEvent;
var
  Callback: TNotifyEvent;
begin
  Callback := Self.OnTimer;

  Check(not Self.Timer.ScheduledEvent(Callback),
        'Empty timer');

  Self.Timer.AddEvent(1000, Callback);

  Check(Self.Timer.ScheduledEvent(Callback),
        'Scheduled event doesn''t appear as scheduled');

  Check(not Self.Timer.ScheduledEvent(Self.WaitEvent),
        'TEvent not yet scheduled');

  Self.Timer.AddEvent(1000, Self.WaitEvent);

  Check(Self.Timer.ScheduledEvent(Self.WaitEvent),
        'TEvent scheduled');
end;

procedure TestTIdDebugTimerQueue.TestTriggerEarliestEvent;
begin
  Self.Timer.AddEvent(1000, Self.OnTimer);
  Self.Timer.AddEvent(2000, Self.OnSecondTimer);

  Self.Timer.TriggerEarliestEvent;

  Check(Self.OnTimerFired, 'TNotifyEvent not triggered');
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
  Self.ErrorSource := TIdFooEvent.Create;
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
