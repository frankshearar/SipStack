unit TestFrameworkEx;

interface

uses
  Classes, SyncObjs, SysUtils, TestFramework;

type
  TThreadingTestCase = class(TTestCase)
  protected
    ExceptionType:    ExceptClass;
    ExceptionMessage: String;
    fDefaultTimeout:  Cardinal;
    ThreadEvent:      TEvent;

    procedure WaitForSignaled; overload;
    procedure WaitForSignaled(Event: TEvent); overload;
  public
    procedure CheckEquals(Expected, Received: TStrings; Msg: String); overload;
    procedure SetUp; override;
    procedure TearDown; override;

    property DefaultTimeout: Cardinal read fDefaultTimeout write fDefaultTimeout;
  end;

implementation

uses
  Math;

//*******************************************************************************
//* TThreadingTestCase                                                          *
//*******************************************************************************
//* TThreadingTestCase Public methods *******************************************

procedure TThreadingTestCase.CheckEquals(Expected, Received: TStrings; Msg: String);
var
  I: Integer;
begin
  for I := 0 to Min(Expected.Count, Received.Count) - 1 do
    if (Msg = '') then
      CheckEquals(Expected[I], Received[I], 'Line ' + IntToStr(I + 1))
    else
      CheckEquals(Expected[I], Received[I], Msg + ': Line ' + IntToStr(I + 1));

  if (Msg = '') then
    CheckEquals(Expected.Count, Received.Count, 'Wrong linecount')
  else
    CheckEquals(Expected.Count, Received.Count, Msg + ': Wrong linecount');
end;

procedure TThreadingTestCase.SetUp;
begin
  inherited SetUp;

  Self.DefaultTimeout   := 5000;
  Self.ExceptionType    := Exception;
  Self.ExceptionMessage := 'The event waited for was never fired';
  Self.ThreadEvent      := TEvent.Create(nil, true, false, 'ThreadEvent');
end;

procedure TThreadingTestCase.TearDown;
begin
  Self.ThreadEvent.Free;

  inherited TearDown;
end;

//* TThreadingTestCase Protected methods ***************************************

procedure TThreadingTestCase.WaitForSignaled;
begin
  Self.WaitForSignaled(Self.ThreadEvent);
end;

procedure TThreadingTestCase.WaitForSignaled(Event: TEvent);
begin
  if (wrSignaled <> Event.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);

  Event.ResetEvent;
end;

end.
