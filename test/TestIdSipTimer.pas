unit TestIdSipTimer;

interface

uses
  IdSipTimer, IdThread, SysUtils, TestFrameworkEx;

type
  TestTIdSipTimer = class(TThreadingTestCase)
  private
    Tick:  Boolean;
    Timer: TIdSipTimer;
    procedure OnException(AThread: TIdThread; AException: Exception);
    procedure OnRaiseExceptionTimer(Sender: TObject);
    procedure OnTickTimer(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExceptionHandling;
    procedure TestTick;
    procedure TestReset;
  end;

implementation

uses
  IdGlobal, SyncObjs, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTimer unit tests');
  Result.AddTest(TestTIdSipTimer.Suite);
end;

//******************************************************************************
//* TestTIdSipTimer                                                            *
//******************************************************************************
//* TestTIdSipTimer Public methods *********************************************

procedure TestTIdSipTimer.SetUp;
begin
  inherited SetUp;

  Self.Tick  := false;
  Self.Timer := TIdSipTimer.Create(true);

  Self.DefaultTimeout := 500;
end;

procedure TestTIdSipTimer.TearDown;
begin
  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

  inherited TearDown;
end;

//* TestTIdSipTimer Private methods ********************************************

procedure TestTIdSipTimer.OnException(AThread: TIdThread; AException: Exception);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSipTimer.OnRaiseExceptionTimer(Sender: TObject);
begin
  raise Exception.Create('OnRaiseExceptionTimer');
end;

procedure TestTIdSipTimer.OnTickTimer(Sender: TObject);
begin
  Self.Tick := true;
  Self.ThreadEvent.SetEvent;
end;

//* TestTIdSipTimer Published methods ******************************************

procedure TestTIdSipTimer.TestExceptionHandling;
begin
  Self.ExceptionMessage := 'Main thread never heard of the exception';

  Self.Timer.OnException := Self.OnException;
  Self.Timer.OnTimer     := Self.OnRaiseExceptionTimer;
  Self.Timer.Interval    := 100;
  Self.Timer.Start;

  Self.WaitForSignaled;
end;

procedure TestTIdSipTimer.TestTick;
begin
  Self.ExceptionMessage := 'OnTick didn''t fire';
  Self.Timer.OnTimer := Self.OnTickTimer;

  Self.Timer.Interval := 100;
  Self.Timer.Start;

  Self.WaitForSignaled;
end;

procedure TestTIdSipTimer.TestReset;
begin
  Self.Timer.OnTimer := Self.OnTickTimer;
  Self.Timer.Interval := 1000;
  Self.Timer.Start;
  Self.WaitForTimeout('Ticked prematurely before Reset');
//  IdGlobal.Sleep(500);
//  Check(not Self.Tick, 'Ticked prematurely before Reset');
  Self.Timer.Reset;
  Self.WaitForTimeout('Ticked prematurely after Reset');
//  IdGlobal.Sleep(500);
//  Check(not Self.Tick, 'Ticked prematurely after Reset');
end;

initialization
  RegisterTest('SIP Timer class', Suite);
end.
