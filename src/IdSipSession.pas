unit IdSipSession;

interface

uses
  IdSipMessage;

type
  TIdSipDialogs = class(TObject); // stub - move to IdSipDialog

  TIdSipSession = class(TObject)
  private
    fDialogs:        TIdSipDialogs;
    fInitialRequest: TIdSipRequest;
  public
    procedure Close;

    property InitialRequest: TIdSipRequest read fInitialRequest;
    property Dialogs:        TIdSipDialogs read fDialogs;
  end;

implementation

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

procedure TIdSipSession.Close;
begin
end;

end.
