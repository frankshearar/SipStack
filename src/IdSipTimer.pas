unit IdSipTimer;

interface

uses
  Classes, IdThread;

type
  TIdSipTimer = class(TIdThread)
  private
    fInterval:  Cardinal;
    fOnTimer:   TNotifyEvent;
    fStart:     TDateTime;
    Resolution: Cardinal;
  protected
    procedure Run; override;
  public
    constructor Create(ACreateSuspended: Boolean = True); override;

    function  ElapsedTime: TDateTime;
    procedure Reset;

    property Interval: Cardinal     read fInterval write fInterval;
    property OnTimer:  TNotifyEvent read fOnTimer write fOnTimer;
  end;

implementation

uses
  DateUtils, SysUtils;

//******************************************************************************
//* TIdSipTimer                                                                *
//******************************************************************************
//* TIdSipTimer Public methods *************************************************

constructor TIdSipTimer.Create(ACreateSuspended: Boolean = True);
begin
  Self.Resolution := 50;

  inherited Create(ACreateSuspended);
end;

function TIdSipTimer.ElapsedTime: TDateTime;
begin
  Result := Now - Self.fStart
end;

procedure TIdSipTimer.Reset;
begin
  Self.fStart := Now;
end;

//* TIdSipTimer Protected methods **********************************************

procedure TIdSipTimer.Run;
begin
  Self.Reset;
  while not Self.Terminated do begin
    Sleep(Self.Resolution);

    if (Self.ElapsedTime > (OneMillisecond * Self.Interval)) then begin
      Self.Reset;
      Self.OnTimer(Self);
    end;
  end;
end;

end.
