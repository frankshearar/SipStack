unit IdSipTransport;

interface

uses
  IdSipMessage, SysUtils;

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
    fACKCount:  Cardinal;
    fLastACK:   TIdSipRequest;
    fFailWith:  ExceptClass;
    fSentCount: Cardinal;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure FireOnResponse(const R: TIdSipResponse);
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentCount;
    procedure SendACK(const R: TIdSipResponse); override;
    procedure SendRequest(const R: TIdSipRequest); override;

    property ACKCount:  Cardinal      read fACKCount;
    property FailWith:  ExceptClass   read fFailWith write fFailWith;
    property LastACK:   TIdSipRequest read fLastACK;
    property SentCount: Cardinal      read fSentCount;
  end;

implementation

uses
  IdSipParser;

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
  Self.fLastACK := TIdSipRequest.Create;
end;

destructor TIdSipMockTransport.Destroy;
begin
  Self.fLastACK.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(R);
end;

procedure TIdSipMockTransport.RaiseException(const E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
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
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendACK');

  Inc(Self.fACKCount);
end;

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if (R.Method = MethodAck) then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else
    Inc(Self.fSentCount);
end;

end.
