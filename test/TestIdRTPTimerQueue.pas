unit TestIdRTPTimerQueue;

interface

uses
  Classes, IdRTPTimerQueue, SyncObjs, SysUtils, TestFramework;

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


  TestTIdRTPTimerQueue = class(TTestCase)
  private
    EventOne:         TEvent;
    EventTwo:         TEvent;
    ExceptionMessage: String;
    ExceptionType:    ExceptClass;
    Lock:             TCriticalSection;
    OrderOfFire:      String;
    Queue:            TIdRTPTimerQueue;
    T1:               TThreadEvent;
    T2:               TThreadEvent;

    procedure OnEventOneSet(Sender: TObject);
    procedure OnEventTwoSet(Sender: TObject);
    procedure WaitFor(Event: TEvent);
    procedure WaitForAll(Events: array of TEvent;
                         Timeout: Cardinal);
    procedure WaitForTimeout(Event: TEvent);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEarliestEvent;
    procedure TestBefore;
    procedure TestOneEvent;
    procedure TestTwoEvents;
    procedure TestTwoOutOfOrderEvents;
  end;

const
  DefaultTimeout = 10;
  LongTimeout    = 10*DefaultTimeout;

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
end;

//******************************************************************************
//* TestTIdRTPTimerQueue                                                       *
//******************************************************************************
//* TestTIdRTPTimerQueue Public methods ****************************************

procedure TestTIdRTPTimerQueue.SetUp;
begin
  inherited SetUp;

  Self.ExceptionType    := Exception;
  Self.ExceptionMessage := 'The event waited for was never fired';
  Self.OrderOfFire      := '';

  Self.EventOne := TSimpleEvent.Create;
  Self.EventTwo := TSimpleEvent.Create;
  Self.Lock     := TCriticalSection.Create;
  Self.Queue    := TIdRTPTimerQueue.Create;

  Self.T1 := TThreadEvent.Create(Self.EventOne,
                                 Self.OnEventOneSet,
                                 LongTimeout);
  Self.T2 := TThreadEvent.Create(Self.EventTwo,
                                 Self.OnEventTwoSet,
                                 LongTimeout);
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

  inherited TearDown;
end;

//* TestTIdRTPTimerQueue Private methods ***************************************

procedure TestTIdRTPTimerQueue.OnEventOneSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '1'
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdRTPTimerQueue.OnEventTwoSet(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    Self.OrderOfFire := Self.OrderOfFire + '2'
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdRTPTimerQueue.WaitFor(Event: TEvent);
begin
  if (wrSignaled <> Event.WaitFor(LongTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
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
    Self.WaitFor(Events[I]);
  end;

  Self.ExceptionMessage := Msg;
end;

procedure TestTIdRTPTimerQueue.WaitForTimeout(Event: TEvent);
begin
  if (wrTimeout <> Self.EventOne.WaitFor(LongTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

//* TestTIdRTPTimerQueue Published methods *************************************

procedure TestTIdRTPTimerQueue.TestEarliestEvent;
var
  EventThree, EventFour: TEvent;
begin
  EventThree := TSimpleEvent.Create;
  try
    EventFour := TSimpleEvent.Create;
    try
    Check(Self.Queue.EarliestEvent = nil, 'Empty list');
    Self.Queue.AddEvent(100, Self.EventOne);
    Check(Self.Queue.EarliestEvent.Event = Self.EventOne,
          'One item');

    Self.Queue.AddEvent(50, Self.EventTwo);
    Check(Self.Queue.EarliestEvent.Event = Self.EventTwo,
          'An earlier item added');

    Self.Queue.AddEvent(150, EventThree);
    Check(Self.Queue.EarliestEvent.Event = Self.EventTwo,
          'A later item added');

   Self.Queue.AddEvent($ffffff00, EventFour);
    Check(Self.Queue.EarliestEvent.Event = EventFour,
          'A really early item added');
    finally
      EventFour.Free;
    end;
  finally
    EventThree.Free;
  end;
end;

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

procedure TestTIdRTPTimerQueue.TestOneEvent;
begin
  Self.ExceptionMessage := 'Event shouldn''t fire';
  Self.Queue.AddEvent(DefaultTimeout, Self.EventOne);
  Self.WaitForTimeout(Self.EventOne);

  Self.ExceptionMessage := 'Event didn''t fire';
  Self.Queue.Start;
  try
    Self.WaitFor(Self.EventOne);
  finally
    Self.Queue.Stop;
  end;
end;

procedure TestTIdRTPTimerQueue.TestTwoEvents;
begin
  Self.Queue.AddEvent(DefaultTimeout, Self.EventOne);
  Self.Queue.AddEvent(2*DefaultTimeout, Self.EventTwo);
  Self.Queue.Start;
  try
    Self.WaitForAll([Self.EventOne, Self.EventTwo],
                    LongTimeout);

    // We have to sleep because although Queue has set both events, the two
    // wait-threads have not yet executed their callbacks!
    IdGlobal.Sleep(50);
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
  Self.Queue.AddEvent(2*DefaultTimeout, Self.EventOne);
  Self.Queue.AddEvent(DefaultTimeout,   Self.EventTwo);
  Self.Queue.Start;
  try
    Self.WaitForAll([Self.EventOne, Self.EventTwo], LongTimeout);

    // We have to sleep because although Queue has set both events, the two
    // wait-threads have not yet executed their callbacks!
    IdGlobal.Sleep(50);
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

initialization
  RegisterTest('IdRTPTimerQueue', Suite);
end.
