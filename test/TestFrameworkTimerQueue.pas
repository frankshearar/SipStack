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

  TLoggingWait = class(TIdWait)
  private
    fBinaryData:        String;
    fDescription:       String;
    fRefCode:           Cardinal;
    fSourceDescription: String;
    fSourceRef:         Cardinal;
    fVerbosityLevel:    Byte;
  protected
    procedure LogTrigger; override;
  public
    property BinaryData:        String   read fBinaryData write fBinaryData;
    property Description:       String   read fDescription write fDescription;
    property RefCode:           Cardinal read fRefCode write fRefCode;
    property SourceDescription: String   read fSourceDescription write fSourceDescription;
    property SourceRef:         Cardinal read fSourceRef write fSourceRef;
    property VerbosityLevel:    Byte     read fVerbosityLevel write fVerbosityLevel;
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

//******************************************************************************
//* TLoggingWait                                                               *
//******************************************************************************
//* TLoggingWait Protected methods *********************************************

procedure TLoggingWait.LogTrigger;
begin
  Self.OnLog(Self.VerbosityLevel, Self.SourceRef, Self.SourceDescription, Self.RefCode, Self.Description, Self.BinaryData);
end;

end.
