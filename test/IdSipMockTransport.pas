unit IdSipMockTransport;

interface

uses
  IdSipHeaders, IdSipMessage, IdSipTransport, IdSocketHandle, SysUtils;

type
  TIdSipMockTransport = class(TIdSipTransport)
  private
    fACKCount:          Cardinal;
    fBindings:          TIdSocketHandles;
    fFailWith:          ExceptClass;
    fLastACK:           TIdSipRequest;
    fLastRequest:       TIdSipRequest;
    fLastResponse:      TIdSipResponse;
    fLocalEchoMessages: Boolean;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fTransportType:     TIdSipTransportType;
  protected
    function  GetBindings: TIdSocketHandles; override;
    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    function  SentByIsRecognised(const Via: TIdSipViaHeader): Boolean; override;
  public
    constructor Create(const Port: Cardinal); override;
    destructor  Destroy; override;

    procedure FireOnRequest(const R: TIdSipRequest);
    procedure FireOnResponse(const R: TIdSipResponse);
    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    function  IsSecure: Boolean; override;
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    procedure Start; override;
    procedure Stop; override;

    property ACKCount:          Cardinal            read fACKCount;
    property FailWith:          ExceptClass         read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest       read fLastACK;
    property LastRequest:       TIdSipRequest       read fLastRequest;
    property LastResponse:      TIdSipResponse      read fLastResponse;
    property LocalEchoMessages: Boolean             read fLocalEchoMessages write fLocalEchoMessages;
    property SentRequestCount:  Cardinal            read fSentRequestCount;
    property SentResponseCount: Cardinal            read fSentResponseCount;
    property TransportType:     TIdSipTransportType read fTransportType write fTransportType;
  end;

implementation

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

constructor TIdSipMockTransport.Create(const Port: Cardinal);
begin
  inherited Create(Port);

  Self.ResetSentRequestCount;
  Self.fBindings     := TIdSocketHandles.Create(nil);
  Self.fLastACK      := TIdSipRequest.Create;
  Self.fLastRequest  := TIdSipRequest.Create;
  Self.fLastResponse := TIdSipResponse.Create;

  Self.LocalEchoMessages := false;
end;

destructor TIdSipMockTransport.Destroy;
begin
  Self.LastResponse.Free;
  Self.LastRequest.Free;
  Self.LastACK.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransport.FireOnRequest(const R: TIdSipRequest);
begin
  Self.NotifyTransportListeners(R);
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.NotifyTransportListeners(R);
end;

function TIdSipMockTransport.GetTransportType: TIdSipTransportType;
begin
  Result := Self.TransportType;
end;

function TIdSipMockTransport.IsReliable: Boolean;
begin
  Result := Self.TransportType <> sttUDP;
end;

function TIdSipMockTransport.IsSecure: Boolean;
begin
  Result := Self.TransportType = sttTLS;
end;

procedure TIdSipMockTransport.RaiseException(const E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
end;

procedure TIdSipMockTransport.ResetACKCount;
begin
  Self.fACKCount := 0;
end;

procedure TIdSipMockTransport.ResetSentRequestCount;
begin
  Self.fSentRequestCount := 0;
end;

procedure TIdSipMockTransport.ResetSentResponseCount;
begin
  Self.fSentResponseCount := 0;
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

//* TIdSipMockTransport Protected methods **************************************

function TIdSipMockTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.fBindings;
end;

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest);
begin
  inherited SendRequest(R);

  if R.IsAck then
    Self.LastACK.Assign(R);

  Self.LastRequest.Assign(R);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if R.IsAck then 
    Inc(Self.fACKCount)
  else
    Inc(Self.fSentRequestCount);

  if Self.LocalEchoMessages then
    Self.NotifyTransportListeners(R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  inherited SendResponse(R);

  Self.LastResponse.Assign(R);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Inc(Self.fSentResponseCount);

  if Self.LocalEchoMessages then
    Self.NotifyTransportListeners(R);
end;

function TIdSipMockTransport.SentByIsRecognised(const Via: TIdSipViaHeader): Boolean;
begin
  Result := true;
end;

end.
