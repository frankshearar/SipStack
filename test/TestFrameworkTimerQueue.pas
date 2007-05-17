unit TestFrameworkTimerQueue;

interface

uses
  IdTimerQueue, SysUtils;

type
  TExceptionRaisingWait = class(TIdWait)
  private
    fErrorMessage: String;
    fErrorType:    ExceptClass;
  public
    constructor Create; override;

    procedure Trigger; override;

    property ErrorMessage: String      read fErrorMessage write fErrorMessage;
    property ErrorType:    ExceptClass read fErrorType write fErrorType;
  end;

implementation

//******************************************************************************
//* TExceptionRaisingWait                                                      *
//******************************************************************************
//* TExceptionRaisingWait Public methods ***************************************

constructor TExceptionRaisingWait.Create;
begin
  inherited Create;

  Self.ErrorMessage := 'Access Violation Occurred Nowhere';
  Self.ErrorType    := EAccessViolation;
end;

procedure TExceptionRaisingWait.Trigger;
begin
  raise Self.ErrorType.Create(Self.ErrorMessage);
end;

end.
