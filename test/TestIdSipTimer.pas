{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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

  TestTIdSipSingleShotTimer = class(TThreadingTestCase)
  private
    ReturnedData: TObject;

    procedure OnTimer(Sender: TObject);
  public
    procedure SetUp; override;
  published
    procedure TestAnEvent;
    procedure TestNoEvent;
  end;

implementation

uses
  IdGlobal, SyncObjs, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTimer unit tests');
  Result.AddTest(TestTIdSipTimer.Suite);
  Result.AddTest(TestTIdSipSingleShotTimer.Suite);
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

//******************************************************************************
//* TestTIdSipSingleShotTimer                                                  *
//******************************************************************************
//* TestTIdSipSingleShotTimer Public methods ***********************************

procedure TestTIdSipSingleShotTimer.SetUp;
begin
  inherited SetUp;

  Self.DefaultTimeout := 200;
end;

//* TestTIdSipSingleShotTimer Private methods **********************************

procedure TestTIdSipSingleShotTimer.OnTimer(Sender: TObject);
begin
  Self.ReturnedData := (Sender as TIdSipSingleShotTimer).Data;

  Self.ThreadEvent.SetEvent;
end;

//* TestTIdSipSingleShotTimer Published methods ********************************

procedure TestTIdSipSingleShotTimer.TestAnEvent;
var
  Data: TObject;
begin
  Self.ExceptionMessage := 'Waiting for single-shot';

  Data := TObject.Create;
  try
    TIdSipSingleShotTimer.Create(Self.OnTimer,
                                 Self.DefaultTimeout div 2,
                                 Data);
    Self.WaitForSignaled;

    Check(Self.ReturnedData = Data,
          'Timer didn''t return the data we gave it');
  finally
    Data.Free;
  end;
end;

procedure TestTIdSipSingleShotTimer.TestNoEvent;
begin
  Self.DefaultTimeout := 200;
  TIdSipSingleShotTimer.Create(nil, Self.DefaultTimeout div 2);
  Self.WaitForTimeout('No event = timeout');
end;


initialization
  RegisterTest('SIP Timer class', Suite);
end.
