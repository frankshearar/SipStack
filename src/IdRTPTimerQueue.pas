unit IdRTPTimerQueue;

interface

uses
  Classes, Contnrs, IdThread, SyncObjs;

type
  TIdRTPEventWait = class(TObject)
  private
    fTriggerTime: Cardinal;
    fEvent:       TEvent;
  public
    property TriggerTime: Cardinal read fTriggerTime write fTriggerTime;
    property Event:       TEvent   read fEvent write fEvent;

    function  Due: Boolean;
    function  TimeToWait: Cardinal;
    procedure Trigger;
  end;

  // I supply a single timer thread with which you may register a wait time
  // and an event. When that wait time is up, I set the event you've given
  // me.
  // You may safely specify a wait time of up to 1 day (86 400 000ms).
  TIdRTPTimerQueue = class(TIdThread)
  private
    EventList: TObjectList;
    Lock:      TCriticalSection;

    function EventAt(Index: Integer): TIdRTPEventWait;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;
    destructor  Destroy; override;

    procedure AddEvent(MillisecsWait: Cardinal;
                       Event: TEvent);
    function  EarliestEvent: TIdRTPEventWait;
    function  Before(TimeA,
                     TimeB: Cardinal): Boolean;
  end;

implementation

uses
  IdGlobal, IdRTP, SysUtils;

//******************************************************************************
//* TIdRTPEventWait                                                            *
//******************************************************************************
//* TIdRTPEventWait Public methods *********************************************

function TIdRTPEventWait.Due: Boolean;
begin
  Result := GetTickCount >= Self.TriggerTime;
end;

function TIdRTPEventWait.TimeToWait: Cardinal;
begin
  if Self.Due then
    Result := 0
  else
    Result := Self.TriggerTime - GetTickCount;
end;

procedure TIdRTPEventWait.Trigger;
begin
  Self.Event.SetEvent;
end;

//******************************************************************************
//* TIdRTPTimerQueue                                                           *
//******************************************************************************
//* TIdRTPTimerQueue Public methods ********************************************

constructor TIdRTPTimerQueue.Create(ACreateSuspended: Boolean = True);
begin
  inherited Create(ACreateSuspended);

  Self.EventList := TObjectList.Create(true);
  Self.Lock      := TCriticalSection.Create;
end;

destructor TIdRTPTimerQueue.Destroy;
begin
  Self.EventList.Free;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TIdRTPTimerQueue.AddEvent(MillisecsWait: Cardinal;
                                    Event: TEvent);
var
  EventWait: TIdRTPEventWait;
begin
  Self.Lock.Acquire;
  try
    EventWait := TIdRTPEventWait.Create;
    try
      Self.EventList.Add(EventWait);
      EventWait.Event       := Event;
      EventWait.TriggerTime := AddModulo(GetTickCount,
                                         MillisecsWait,
                                         High(MillisecsWait));
    except
      Self.EventList.Remove(EventWait);
      raise;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdRTPTimerQueue.EarliestEvent: TIdRTPEventWait;
var
  I: Integer;
begin
  if (Self.EventList.Count = 0) then begin
    Result := nil;
    Exit;
  end;

  Self.Lock.Acquire;
  try
    Result := Self.EventAt(0);
    for I := 1 to Self.EventList.Count - 1 do
      if Self.Before(Self.EventAt(I).TriggerTime, Result.TriggerTime) then
        Result := Self.EventAt(I);
  finally
    Self.Lock.Release;
  end;
end;

function TIdRTPTimerQueue.Before(TimeA,
                                 TimeB: Cardinal): Boolean;
begin
  // You can't use Before() to determine the ordering of ticks
  // if the ticks are more than a day apart (i.e., MSecsPerDay ticks).
  Result := GetTickDiff(TimeA, TimeB) < MSecsPerDay;
end;

//* TIdRTPTimerQueue Protected methods *****************************************

procedure TIdRTPTimerQueue.Run;
var
  NextEvent: TIdRTPEventWait;
begin
  while not Terminated do begin
    NextEvent := Self.EarliestEvent;

    if Assigned(NextEvent) then begin
      if not NextEvent.Due then
        IdGlobal.Sleep(NextEvent.TimeToWait);

      NextEvent.Trigger;
      Self.EventList.Remove(NextEvent)
    end
    else
      IdGlobal.Sleep(100);
  end;
end;

//* TIdRTPTimerQueue Private methods *******************************************

function TIdRTPTimerQueue.EventAt(Index: Integer): TIdRTPEventWait;
begin
  Result := Self.EventList[Index] as TIdRTPEventWait;
end;

end.
