unit TestIdSipTimer;

interface

uses
  IdSipTimer, IdThread, SysUtils, TestFramework;

type
  TestTIdSipTimer = class(TTestCase)
  private
    GotException:    Boolean;
    Tick:            Boolean;
    TickAccumulator: Cardinal;
    Timer:           TIdSipTimer;
    procedure OnAccumulatorTimer(Sender: TObject);
    procedure OnException(AThread: TIdThread; AException: Exception);
    procedure OnRaiseExceptionTimer(Sender: TObject);
    procedure OnTickTimer(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExceptionHandling;
    procedure TestMultipleTicks;
    procedure TestTick;
    procedure TestReset;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTimer unit tests');
  Result.AddTest(TestTIdSipTimer.Suite);
end;

//******************************************************************************
//* TestTIdSipTimer                                                 *
//******************************************************************************
//* TestTIdSipTimer Public methods **********************************

procedure TestTIdSipTimer.SetUp;
begin
  GotException    := false;
  Tick            := false;
  TickAccumulator := 0;
  Timer           := TIdSipTimer.Create(true);
end;

procedure TestTIdSipTimer.TearDown;
begin
  Timer.Stop;
  Timer.WaitFor;
  Timer.Free;
end;

//* TestTIdSipTimer Private methods *********************************

procedure TestTIdSipTimer.OnAccumulatorTimer(Sender: TObject);
begin
  Inc(TickAccumulator);
  Self.Check(Self.Timer = Sender, 'Unknown Sender');
end;

procedure TestTIdSipTimer.OnException(AThread: TIdThread; AException: Exception);
begin
  GotException := true;
end;

procedure TestTIdSipTimer.OnRaiseExceptionTimer(Sender: TObject);
begin
  raise Exception.Create('OnRaiseExceptionTimer');
end;

procedure TestTIdSipTimer.OnTickTimer(Sender: TObject);
begin
  Tick := true;
  Self.Check(Self.Timer = Sender, 'Unknown Sender');
end;

//* TestTIdSipTimer Published methods *******************************

procedure TestTIdSipTimer.TestExceptionHandling;
begin
  Timer.OnException := Self.OnException;
  Timer.OnTimer     := Self.OnRaiseExceptionTimer;
  Timer.Interval    := 100;
  Timer.Start;
  Sleep(375);

  Check(GotException, 'Main thread never heard of the exception');
end;

procedure TestTIdSipTimer.TestMultipleTicks;
begin
  Timer.OnTimer := Self.OnAccumulatorTimer;

  Timer.Interval   := 100;
  Timer.Start;
  Sleep(375);

  CheckEquals(3, TickAccumulator, 'Unexpected number of ticks');
end;

procedure TestTIdSipTimer.TestTick;
begin
  Timer.OnTimer := Self.OnTickTimer;

  Timer.Interval := 100;
  Timer.Start;
  Sleep(150);

  Check(Tick, 'Event didn''t fire');
end;

procedure TestTIdSipTimer.TestReset;
begin
  Timer.OnTimer := Self.OnTickTimer;
  Timer.Interval := 1000;
  Timer.Start;
  Sleep(500);

  Check(not Tick, 'Ticked prematurely before Reset');
  Timer.Reset;
  Sleep(500);
  Check(not Tick, 'Ticked prematurely after Reset');
end;

initialization
  RegisterTest('SIP Timer class', Suite);
end.
