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
  Classes, IdTimerQueue, SyncObjs, SysUtils, TestFramework, TestFrameworkEx;

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

  TestTIdTimerQueue = class(TThreadingTestCase)
  private
    CallbackEventOne: TEvent;
    CallbackEventTwo: TEvent;
    EventOne:         TEvent;
    EventTwo:         TEvent;
    Lock:             TCriticalSection;
    Notified:         Boolean;
    OrderOfFire:      String;
    Queue:            TIdTimerQueue;
    T1:               TThreadEvent;
    T2:               TThreadEvent;

    procedure CheckNotifyEvent(Sender: TObject);
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
    procedure TestBefore;
    procedure TestNotifyEvent;
    procedure TestOneEvent;
    procedure TestRemoveEvent;
    procedure TestRemoveNonExistentEvent;
    procedure TestRemoveNonExistentEventWithOtherEvents;
    procedure TestRemoveNonExistentNotifyEventWithOtherNotifyEvents;
    procedure TestRemoveNotifyEvent;
    procedure TestTwoEvents;
    procedure TestTwoNotifyEvents;
    procedure TestTwoOutOfOrderEvents;
    procedure TestWaitForEarliestEvent;
  end;

const
  ShortTimeout = 10;
  LongTimeout  = 10*ShortTimeout;

implementation

uses
  IdGlobal;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPTimerQueue unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdTimerQueue.Suite);
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
//* TestTIdTimerQueue                                                          *
//******************************************************************************
//* TestTIdTimerQueue Public methods *******************************************

procedure TestTIdTimerQueue.SetUp;
begin
  inherited SetUp;

  Self.DefaultTimeout   := 100;
  Self.ExceptionType    := Exception;
  Self.ExceptionMessage := 'The event waited for was never fired';
  Self.Notified         := false;
  Self.OrderOfFire      := '';

  Self.CallbackEventOne := TSimpleEvent.Create;
  Self.CallbackEventTwo := TSimpleEvent.Create;
  Self.EventOne         := TSimpleEvent.Create;
  Self.EventTwo         := TSimpleEvent.Create;
  Self.Lock             := TCriticalSection.Create;
  Self.Queue            := TIdTimerQueue.Create(false);

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

procedure TestTIdTimerQueue.TearDown;
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
  Self.CallbackEventTwo.Free;
  Self.CallbackEventOne.Free;

  inherited TearDown;
end;

//* TestTIdTimerQueue Private methods ******************************************

procedure TestTIdTimerQueue.CheckNotifyEvent(Sender: TObject);
begin
  Self.Notified := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdTimerQueue.NotifyEventOne(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'a';
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdTimerQueue.NotifyEventTwo(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'b';
  finally
    Self.Lock.Release;
  end;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdTimerQueue.OnEventOneSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '1';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventOne.SetEvent;
end;

procedure TestTIdTimerQueue.OnEventTwoSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '2';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventTwo.SetEvent;
end;

procedure TestTIdTimerQueue.WaitForAll(Events: array of TEvent;
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

//* TestTIdTimerQueue Published methods ****************************************

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

procedure TestTIdTimerQueue.TestNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout, Self.CheckNotifyEvent);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.ThreadEvent);
  Check(Self.Notified, 'TNotifyEvent didn''t fire');
end;

procedure TestTIdTimerQueue.TestOneEvent;
begin
  TThreadEvent.Create(Self.EventOne,
                      Self.OnEventOneSet,
                      LongTimeout);

  Self.Queue.AddEvent(ShortTimeout, Self.EventOne);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.EventOne);
end;

procedure TestTIdTimerQueue.TestRemoveEvent;
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

procedure TestTIdTimerQueue.TestRemoveNonExistentEvent;
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

procedure TestTIdTimerQueue.TestRemoveNonExistentEventWithOtherEvents;
begin
  // This catches a bug where if there were multiple TEvents
  // and you removed a non-existent one you'd enter an infinite loop.
  // Simple implementation bug.

  Self.Queue.AddEvent(ShortTimeout, Self.EventOne);
  Self.Queue.RemoveEvent(Self.EventTwo);
end;

procedure TestTIdTimerQueue.TestRemoveNonExistentNotifyEventWithOtherNotifyEvents;
begin
  // This catches a bug where if there were multiple TNotifyEvents
  // and you removed a non-existent one you'd enter an infinite loop.
  // Simple implementation bug.

  Self.Queue.AddEvent(ShortTimeout, Self.NotifyEventOne);
  Self.Queue.RemoveEvent(Self.NotifyEventTwo);
end;

procedure TestTIdTimerQueue.TestRemoveNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.CheckNotifyEvent);
  Self.Queue.AddEvent(2*ShortTimeout, Self.CheckNotifyEvent);
  Self.Queue.AddEvent(3*ShortTimeout, Self.EventOne);
  Self.Queue.RemoveEvent(Self.CheckNotifyEvent);

  Self.ExceptionMessage := 'Event didn''t fire';

  Self.WaitForSignaled(Self.EventOne);
  Check(not Self.Notified, 'TNotifyEvent wasn''t removed');
end;

procedure TestTIdTimerQueue.TestTwoEvents;
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

procedure TestTIdTimerQueue.TestTwoNotifyEvents;
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

procedure TestTIdTimerQueue.TestTwoOutOfOrderEvents;
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

procedure TestTIdTimerQueue.TestWaitForEarliestEvent;
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

initialization
  RegisterTest('IdRTPTimerQueue', Suite);
end.
