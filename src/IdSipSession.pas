unit IdSipSession;

interface

uses
  IdSipDialog, IdSipMessage;

type
  TIdSipSession = class(TObject)
  private
    fDialogs:        TIdSipDialogs;
    fInitialRequest: TIdSipRequest;
  public
    constructor Create(const InitialRequest: TIdSipRequest);
    destructor  Destroy; override;

    procedure Cancel;
    procedure Modify;
    procedure Terminate;

    property InitialRequest: TIdSipRequest read fInitialRequest;
    property Dialogs:        TIdSipDialogs read fDialogs;
  end;

implementation

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

constructor TIdSipSession.Create(const InitialRequest: TIdSipRequest);
begin
  inherited Create;
end;

destructor TIdSipSession.Destroy;
begin
  inherited Destroy;
end;

procedure TIdSipSession.Cancel;
begin
end;

procedure TIdSipSession.Modify;
begin
end;

procedure TIdSipSession.Terminate;
begin
end;

end.
