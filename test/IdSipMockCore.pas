unit IdSipMockCore;

interface

uses
  IdSipCore, IdSipHeaders, IdSipMessage, IdSipTransaction, IdSipTransport;

type
  TIdSipMockCore = class(TIdSipAbstractCore)
  private
    fReceiveRequestCalled: Boolean;
    fReceiveResponseCalled: Boolean;
  public
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure ReceiveRequest(const Request: TIdSipRequest;
                             const Transaction: TIdSipTransaction;
                             const Transport: TIdSipTransport); override;
    procedure ReceiveResponse(const Response: TIdSipResponse;
                              const Transaction: TIdSipTransaction;
                              const Transport: TIdSipTransport); override;

    procedure Reset;

    property ReceiveRequestCalled:  Boolean read fReceiveRequestCalled;
    property ReceiveResponseCalled: Boolean read fReceiveResponseCalled;
  end;

  TIdSipMockSession = class(TIdSipSession)
  private
    fResponseResent: Boolean;
  public
    constructor Create;

    procedure ResendLastResponse; override;

    property ResponseResent: Boolean read fResponseResent;
  end;

implementation

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

function TIdSipMockCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
var
  UA: TIdSipUserAgentCore;
begin
  UA := TIdSipUserAgentCore.Create;
  try
    Result := UA.CreateRequest(Dest);
  finally
    UA.Free;
  end;
end;

function TIdSipMockCore.CreateResponse(const Request:      TIdSipRequest;
                                       const ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := nil;
end;

procedure TIdSipMockCore.ReceiveRequest(const Request: TIdSipRequest;
                                        const Transaction: TIdSipTransaction;
                                        const Transport: TIdSipTransport);
begin
  fReceiveRequestCalled := true;
end;

procedure TIdSipMockCore.ReceiveResponse(const Response: TIdSipResponse;
                                         const Transaction: TIdSipTransaction;
                                         const Transport: TIdSipTransport);
begin
  fReceiveResponseCalled := true;
end;

procedure TIdSipMockCore.Reset;
begin
  fReceiveRequestCalled  := true;
  fReceiveResponseCalled := true;
end;

//******************************************************************************
//* TIdSipMockSession                                                          *
//******************************************************************************
//* TIdSipMockSession Public methods *******************************************

constructor TIdSipMockSession.Create;
begin
  inherited Create;

  Self.fResponseResent := false;
end;

procedure TIdSipMockSession.ResendLastResponse;
begin
  Self.fResponseResent := true;
end;

end.
