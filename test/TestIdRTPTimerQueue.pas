unit TestIdRTPTimerQueue;

interface

uses
  Classes, IdRTPTimerQueue, SyncObjs, SysUtils, TestFramework, TestFrameworkEx;

type
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

  TestTIdRTPTimerQueue = class(TThreadingTestCase)
  private
    CallbackEventOne: TEvent;
    CallbackEventTwo: TEvent;
    EventOne:         TEvent;
    EventTwo:         TEvent;
    Lock:             TCriticalSection;
    Notified:         Boolean;
    OrderOfFire:      String;
    Queue:            TIdRTPTimerQueue;
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
    procedure TestRemoveNotifyEvent;
    procedure TestTwoNotifyEvents;
    procedure TestTwoEvents;
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
  Result.AddTest(TestTIdRTPTimerQueue.Suite);
end;

//******************************************************************************
//* TThreadEvent                                                               *
//******************************************************************************
//* TThreadEvent Public methods ************************************************

constructor TThreadEvent.Create(Event: TEvent;
                                OnEventSet: TNotifyEvent;
                                MaxWait: Cardinal);
begin
  Self.Event      := Event;
  Self.MaxWait    := MaxWait;
  Self.OnEventSet := OnEventSet;

  inherited Create(false);
end;

//* TThreadEvent Protected methods *********************************************

procedure TThreadEvent.Execute;
begin
  Self.Event.WaitFor(Self.MaxWait);

  if Assigned(Self.OnEventSet) then
    Self.OnEventSet(Self);
    
  Self.Terminate;
end;

//******************************************************************************
//* TestTIdRTPTimerQueue                                                       *
//******************************************************************************
//* TestTIdRTPTimerQueue Public methods ****************************************

procedure TestTIdRTPTimerQueue.SetUp;
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
  Self.Queue            := TIdRTPTimerQueue.Create;

  Self.T1 := TThreadEvent.Create(Self.EventOne,
                                 Self.OnEventOneSet,
                                 LongTimeout);
  Self.T2 := TThreadEvent.Create(Self.EventTwo,
                                 Self.OnEventTwoSet,
                                 LongTimeout);
  Self.T1.Resume;
  Self.T2.Resume;
end;

procedure TestTIdRTPTimerQueue.TearDown;
begin
  Self.T2.Free;
  Self.T1.Free;
  Self.Queue.Stop;
  Self.Queue.Free;
  Self.Lock.Free;
  Self.EventTwo.Free;
  Self.EventOne.Free;
  Self.CallbackEventTwo.Free;
  Self.CallbackEventOne.Free;

  inherited TearDown;
end;

//* TestTIdRTPTimerQueue Private methods ***************************************

procedure TestTIdRTPTimerQueue.CheckNotifyEvent(Sender: TObject);
begin
  Self.Notified := true;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdRTPTimerQueue.NotifyEventOne(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'a';
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdRTPTimerQueue.NotifyEventTwo(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + 'b';
  finally
    Self.Lock.Release;
  end;
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdRTPTimerQueue.OnEventOneSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '1';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventOne.SetEvent;
end;

procedure TestTIdRTPTimerQueue.OnEventTwoSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '2';
  finally
    Self.Lock.Release;
  end;
  Self.CallbackEventTwo.SetEvent;
end;

procedure TestTIdRTPTimerQueue.WaitForAll(Events: array of TEvent;
                                          Timeout: Cardinal);
var
  I:   Integer;
  Msg: String;
begin
  Msg := Self.ExceptionMessage;

  for I := Low(Events) to High(Events) do begin
    Self.ExceptionMessage := Msg + '; Event ' + IntToStr(I);
    Self.WaitForSignaled(Events[I]);
  end;

  Self.ExceptionMessage := Msg;
end;

//* TestTIdRTPTimerQueue Published methods *************************************

procedure TestTIdRTPTimerQueue.TestBefore;
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

procedure TestTIdRTPTimerQueue.TestNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout, Self.CheckNotifyEvent);

  Self.ExceptionMessage := 'Event didn''t fire';
  Self.Queue.Start;
  try
    Self.WaitForSignaled(Self.ThreadEvent);
    Check(Self.Notified, 'TNotifyEvent didn''t fire');
  finally
    Self.Queue.Stop;
  end;
end;

procedure TestTIdRTPTimerQueue.TestOneEvent;
begin
  Self.Queue.AddEvent(ShortTimeout, Self.EventOne);

  Self.ExceptionMessage := 'Event didn''t fire';
  Self.Queue.Start;
  try
    Self.WaitForSignaled(Self.EventOne);
  finally
    Self.Queue.Stop;
  end;
end;

procedure TestTIdRTPTimerQueue.TestRemoveEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.EventOne);
  Self.Queue.AddEvent(ShortTimeout*2, Self.EventOne);
  Self.Queue.AddEvent(ShortTimeout*3, Self.EventTwo);
  Self.Queue.RemoveEvent(Self.EventOne);

  Self.ExceptionMessage := 'Event didn''t fire';
  Self.Queue.Start;
  try
    Self.WaitForSignaled(Self.EventTwo);
    CheckEquals(0,
                Pos('1', Self.OrderOfFire),
                'EventOne wasn''t removed (Order of fire was '
              + Self.OrderOfFire + ')');
  finally
    Self.Queue.Stop;
  end;
end;

procedure TestTIdRTPTimerQueue.TestRemoveNotifyEvent;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.CheckNotifyEvent);
  Self.Queue.AddEvent(ShortTimeout*2, Self.CheckNotifyEvent);
  Self.Queue.AddEvent(ShortTimeout*3, Self.EventOne);
  Self.Queue.RemoveEvent(Self.CheckNotifyEvent);

  Self.ExceptionMessage := 'Event didn''t fire';
  Self.Queue.Start;
  try
    Self.WaitForSignaled(Self.EventOne);
    Check(not Self.Notified, 'TNotifyEvent wasn''t removed');
  finally
    Self.Queue.Stop;
  end;
end;

procedure TestTIdRTPTimerQueue.TestTwoNotifyEvents;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.NotifyEventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.NotifyEventTwo);
  Self.Queue.Start;
  try
    Self.WaitForSignaled(Self.ThreadEvent);
  finally
    Self.Queue.Stop;
  end;

  Self.Lock.Acquire;
  try
    CheckEquals('ab', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdRTPTimerQueue.TestTwoEvents;
begin
  Self.Queue.AddEvent(ShortTimeout,   Self.EventOne);
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventTwo);
  Self.Queue.Start;
  try
    Self.WaitForAll([Self.CallbackEventOne, Self.CallbackEventTwo],
                    LongTimeout);
  finally
    Self.Queue.Stop;
  end;

  Self.Lock.Acquire;
  try
    CheckEquals('12', Self.OrderOfFire, 'Order of events');
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdRTPTimerQueue.TestTwoOutOfOrderEvents;
begin
  Self.Queue.AddEvent(2*ShortTimeout, Self.EventOne);
  Self.Queue.AddEvent(ShortTimeout,   Self.EventTwo);
  Self.Queue.Start;
  try
    Self.WaitForAll([Self.CallbackEventOne, Self.CallbackEventTwo],
                    LongTimeout);
  finally
    Self.Queue.Stop;
  end;

    Self.Lock.Acquire;
    try
      CheckEquals('21', Self.OrderOfFire, 'Order of events');
    finally
      Self.Lock.Release;
    end;
end;

procedure TestTIdRTPTimerQueue.TestWaitForEarliestEvent;
begin
  Self.Queue.Start;
  try
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
  finally
    Self.Queue.Stop;
  end;
end;

initialization
  RegisterTest('IdRTPTimerQueue', Suite);
end.
