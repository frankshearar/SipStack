unit IdSipCore;

interface

uses
  IdSipMessage, IdSipSession;

type
  TIdSipSessionEvent = procedure(const Self: TObject; const Session: TIdSipSession);

  TIdSipAbstractCore = class(TObject)
  private
    fOnNewSession: TIdSipSessionEvent;
  protected
    procedure DoOnNewSession(const Session: TIdSipSession);
  public
    procedure Invite(const R: TIdSipRequest); virtual;

    property OnNewSession: TIdSipSessionEvent read fOnNewSession write fOnNewSession;
  end;

implementation

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

procedure TIdSipAbstractCore.Invite(const R: TIdSipRequest);
begin
  // http://roke.angband.za.org/frank/SessionInitiationProtocol/Section-13_2_1
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.DoOnNewSession(const Session: TIdSipSession);
begin
  if Assigned(Self.OnNewSession) then
    Self.OnNewSession(Self, Session);
end;

end.
