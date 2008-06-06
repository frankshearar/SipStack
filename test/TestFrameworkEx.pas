{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkEx;

interface

uses
  Classes, SyncObjs, SysUtils, TestFramework;

type
  TThreadingTestCase = class(TTestCase)
  protected
    ExceptionMessage: String;
    ExceptionType:    ExceptClass;    
    fDefaultTimeout:  Cardinal;
    ThreadEvent:      TEvent;

    procedure WaitForSignaled; overload;
    procedure WaitForSignaled(Event: TEvent); overload;
    procedure WaitForSignaled(Event: TEvent; Msg: String); overload;
    procedure WaitForSignaled(Msg: String); overload;
    procedure WaitForTimeout(Msg: String); overload;
    procedure WaitForTimeout(Event: TEvent; Msg: String); overload;
  public
    procedure CheckEquals(Expected, Received: TStrings; Msg: String); overload;
    procedure CheckEqualsW(Expected,
                           Actual: WideString;
                           Msg: String);
    procedure CheckUnicode(Expected: WideString;
                           Actual: String;
                           Msg: String);
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

procedure TThreadingTestCase.CheckEqualsW(Expected,
                                          Actual: WideString;
                                          Msg: String);
begin
  if (Expected <> Actual) then
    FailNotEquals(Expected, Actual, Msg, CallerAddr);
end;

procedure TThreadingTestCase.CheckUnicode(Expected: WideString;
                                          Actual: String;
                                          Msg: String);
var
  ActualI:   Integer;
  ExpectedI: Integer;
  W:         WideChar;
begin
  // Check that Actual contains the same data, byte-for-byte, as Expected.

  ActualI   := 1;
  ExpectedI := 1;
  while ActualI <= Length(Actual) - 1 do begin
    W := WideChar((Ord(Actual[ActualI]) shl 8) + Ord(Actual[ActualI + 1]));
    Check(Expected[ExpectedI] = W,
          Msg + ': character ' + IntToStr(ExpectedI)
              + ' ($' + IntToHex(Ord(W), 4) + ') differs');
    Inc(ActualI, SizeOf(WideChar));
    Inc(ExpectedI);
  end;

  CheckEquals(Length(Expected),
              Length(Actual) div SizeOf(WideChar),
              Msg + ': differing lengths');
end;

procedure TThreadingTestCase.SetUp;
const
  FiveSeconds = 5000;
begin
  inherited SetUp;

  Self.DefaultTimeout   := FiveSeconds;
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
  Self.WaitForSignaled(Self.ThreadEvent, Self.ExceptionMessage);
end;

procedure TThreadingTestCase.WaitForSignaled(Event: TEvent);
begin
  Self.WaitForSignaled(Event, Self.ExceptionMessage);
end;

procedure TThreadingTestCase.WaitForSignaled(Event: TEvent; Msg: String);
var
  FullMsg: String;
begin
  if (wrSignaled <> Event.WaitFor(Self.DefaultTimeout)) then begin
    if (Self.ExceptionType <> Exception) then begin
      // We've timed out, and ExceptionType/Message contains details of the
      // exception that resulted in the timeout.
      FullMsg := Format('%s (%s)', [Msg, Self.ExceptionMessage])
    end
    else begin
      // We've timed out, but have no additional information. 
      FullMsg := Msg;
    end;

    raise Self.ExceptionType.Create(FullMsg);
  end;

  Event.ResetEvent;
end;

procedure TThreadingTestCase.WaitForSignaled(Msg: String);
begin
  Self.WaitForSignaled(Self.ThreadEvent, Msg);
end;

procedure TThreadingTestCase.WaitForTimeout(Msg: String);
begin
  Self.WaitForTimeout(Self.ThreadEvent, Msg);
end;

procedure TThreadingTestCase.WaitForTimeout(Event: TEvent; Msg: String);
begin
  if (wrTimeout <> Event.WaitFor(Self.DefaultTimeout)) then
    Fail(Msg);
end;

end.
