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
    function  CreateRequest(Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    procedure ReceiveRequest(Request: TIdSipRequest;
                             Transaction: TIdSipTransaction;
                             Transport: TIdSipTransport); override;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              Transaction: TIdSipTransaction;
                              Transport: TIdSipTransport); override;

    procedure Reset;

    property ReceiveRequestCalled:  Boolean read fReceiveRequestCalled;
    property ReceiveResponseCalled: Boolean read fReceiveResponseCalled;
  end;

  TIdSipMockSession = class(TIdSipSession)
  private
    fResponseResent: Boolean;
  public
    constructor Create(UA: TIdSipUserAgentCore);

    procedure ResendLastResponse; override;

    property ResponseResent: Boolean read fResponseResent;
  end;

implementation

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

function TIdSipMockCore.CreateRequest(Dest: TIdSipToHeader): TIdSipRequest;
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

function TIdSipMockCore.CreateResponse(Request: TIdSipRequest;
                                       ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := nil;
end;

procedure TIdSipMockCore.ReceiveRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Transport: TIdSipTransport);
begin
  fReceiveRequestCalled := true;
end;

procedure TIdSipMockCore.ReceiveResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Transport: TIdSipTransport);
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

constructor TIdSipMockSession.Create(UA: TIdSipUserAgentCore);
begin
  inherited Create(UA);

  Self.fResponseResent := false;
end;

procedure TIdSipMockSession.ResendLastResponse;
begin
  Self.fResponseResent := true;
end;

end.
