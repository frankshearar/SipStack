unit IdSipTransport;

interface

uses
  IdSipMessage;

type
  TIdSipResponseEvent = procedure(Sender: TObject; const R: TIdSipResponse) of object;

  TIdSipAbstractTransport = class(TObject)
  private
    fOnResponse: TIdSipResponseEvent;
  protected
    procedure DoOnResponse(const R: TIdSipResponse);
  public
    // This should raise an appropriate exception if a transport or suchlike
    // error occurs.
    procedure SendACK(const R: TIdSipResponse); virtual; abstract;
    procedure SendRequest(const R: TIdSipRequest); virtual; abstract;

    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
  end;

  TIdSipMockTransport = class(TIdSipAbstractTransport)
  private
    fACKCount: Cardinal;
    fSentCount: Cardinal;
  public
    constructor Create;

    procedure FireOnResponse(const R: TIdSipResponse);
    procedure ResetACKCount;
    procedure ResetSentCount;
    procedure SendACK(const R: TIdSipResponse); override;
    procedure SendRequest(const R: TIdSipRequest); override;

    property ACKCount:  Cardinal read fACKCount;
    property SentCount: Cardinal read fSentCount;
  end;

implementation

//******************************************************************************
//* TIdSipAbstractTransport                                                    *
//******************************************************************************
//* TIdSipAbstractTransport Protected methods **********************************

procedure TIdSipAbstractTransport.DoOnResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R)
end;

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

constructor TIdSipMockTransport.Create;
begin
  inherited Create;

  Self.ResetSentCount;
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(R);
end;

procedure TIdSipMockTransport.ResetACKCount;
begin
  Self.fACKCount := 0;
end;

procedure TIdSipMockTransport.ResetSentCount;
begin
  Self.fSentCount := 0;
end;

procedure TIdSipMockTransport.SendACK(const R: TIdSipResponse);
begin
  Inc(Self.fACKCount);
end;

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest);
begin
  Inc(Self.fSentCount);
end;

end.
