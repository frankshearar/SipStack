{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTimer;

interface

uses
  Classes, IdBaseThread, SyncObjs;

type
  // I represent a recurring timer. Use me whenever you want an event to be
  // triggered over and over at a (more or less) constant interval.
  TIdSipTimer = class(TIdBaseThread)
  private
    fInterval: Cardinal;
    fOnTimer:  TNotifyEvent;
    WaitEvent: TEvent;
  protected
    procedure Run; override;
  public
    constructor Create(CreateSuspended: Boolean = True); reintroduce;
    destructor  Destroy; override;

    procedure Terminate; override;

    property Interval: Cardinal     read fInterval write fInterval;
    property OnTimer:  TNotifyEvent read fOnTimer write fOnTimer;
  end;

  // I provide a one-shot timer. I free myself once I've executed the notify
  // event you supply.
  TIdSipSingleShotTimer = class(TIdBaseThread)
  private
    Event:     TNotifyEvent;
    fData:     TObject;
    WaitEvent: TEvent;
    WaitTime:  Cardinal;
  protected
    procedure Run; override;
  public
    constructor Create(Event: TNotifyEvent;
                       WaitTime: Cardinal;
                       Data: TObject = nil); reintroduce;
    destructor  Destroy; override;

    procedure Terminate; override;


    property Data: TObject read fData;
  end;

implementation

uses
  DateUtils, IdGlobal, SysUtils;

//******************************************************************************
//* TIdSipTimer                                                                *
//******************************************************************************
//* TIdSipTimer Public methods *************************************************

constructor TIdSipTimer.Create(CreateSuspended: Boolean = True);
begin
  Self.WaitEvent := TSimpleEvent.Create;

  inherited Create(CreateSuspended);
end;

destructor TIdSipTimer.Destroy;
begin
  Self.WaitEvent.Free;

  inherited Destroy;
end;

procedure TIdSipTimer.Terminate;
begin
  inherited Terminate;

  Self.WaitEvent.SetEvent;
end;

//* TIdSipTimer Protected methods **********************************************

procedure TIdSipTimer.Run;
begin
  while not Self.Terminated do begin
    Self.WaitEvent.WaitFor(Self.Interval);

    if not Self.Terminated and Assigned(Self.OnTimer) then
      Self.OnTimer(Self);
  end;
end;

//******************************************************************************
//* TIdSipSingleShotTimer                                                      *
//******************************************************************************
//* TIdSipSingleShotTimer Public methods ***************************************

constructor TIdSipSingleShotTimer.Create(Event: TNotifyEvent;
                                         WaitTime: Cardinal;
                                         Data: TObject = nil);
begin
  inherited Create(true);

  Self.FreeOnTerminate := true;
  Self.WaitEvent       := TSimpleEvent.Create;

  Self.Event    := Event;
  Self.fData    := Data;
  Self.WaitTime := WaitTime;

  Self.Resume;
end;

destructor TIdSipSingleShotTimer.Destroy;
begin
  Self.WaitEvent.Free;

  inherited Destroy;
end;

procedure TIdSipSingleShotTimer.Terminate;
begin
  inherited Terminate;

  Self.WaitEvent.SetEvent;
end;

//* TIdSipSingleShotTimer Protected methods ************************************

procedure TIdSipSingleShotTimer.Run;
begin
  Self.WaitEvent.WaitFor(Self.WaitTime);

  if not Self.Terminated and Assigned(Self.Event) then
    Self.Event(Self);
end;

end.
