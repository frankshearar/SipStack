{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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
    procedure AfterRun; virtual;
    procedure BeforeRun; virtual;
    procedure Execute; override;
    procedure Run; virtual;
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

procedure TIdBaseThread.AfterRun;
begin
  // By default do nothing.
end;

procedure TIdBaseThread.BeforeRun;
begin
  // By default do nothing.
end;

procedure TIdBaseThread.Execute;
begin
  try
    Self.BeforeRun;
    try
      Self.Run;
    finally
      Self.AfterRun;
    end;
  except
    on E: Exception do
      Self.DoOnException(E);
  end;
end;

procedure TIdBaseThread.Run;
begin
  // By default, do nothing. If you don't override this method, the thread
  // simply terminates immediately.
end;

//* TIdBaseThread Private methods **********************************************

procedure TIdBaseThread.DoOnException(E: Exception);
begin
  if Assigned(Self.OnException) then
    Self.OnException(Self, E);
end;

end.
