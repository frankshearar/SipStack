unit IdBaseThread;

interface

uses
  Classes, SysUtils;

type
  TIdBaseThread = class;

  TIdExceptionThreadEvent = procedure(Thread: TIdBaseThread;
                                      Exception: Exception) of object;

  TIdBaseThread = class(TThread)
  private
    fOnException: TIdExceptionThreadEvent;

    procedure DoOnException(E: Exception);
  protected
    procedure Execute; override;
    procedure Run; virtual; abstract;
  public
    constructor Create(CreateSuspended: Boolean = True); virtual;

    procedure Terminate; virtual;
    procedure TerminateAndWaitFor; virtual;

    property OnException: TIdExceptionThreadEvent read fOnException write fOnException;
  end;

  EThreadTerminateAndWaitFor = class(Exception);

implementation

//******************************************************************************
//* TIdBaseThread                                                              *
//******************************************************************************
//* TIdBaseThread Public methods ***********************************************

constructor TIdBaseThread.Create(CreateSuspended: Boolean = True);
begin
  inherited Create(CreateSuspended);
end;

procedure TIdBaseThread.Terminate;
begin
  inherited Terminate;
end;

procedure TIdBaseThread.TerminateAndWaitFor;
begin
  if Self.FreeOnTerminate then
    raise EThreadTerminateAndWaitFor.Create('Never TerminateAndWaitFor on a thread that FreeOnTerminates');

  Self.Terminate;
  Self.Resume;
  Self.WaitFor;
end;

//* TIdBaseThread Protected methods ********************************************

procedure TIdBaseThread.Execute;
begin
  try
    Self.Run;
  except
    on E: Exception do
      Self.DoOnException(E);
  end;
end;

//* TIdBaseThread Private methods **********************************************

procedure TIdBaseThread.DoOnException(E: Exception);
begin
  if Assigned(Self.OnException) then
    Self.OnException(Self, E);
end;

end.
