unit IdSipMockCore;

interface

uses
  IdSipCore, IdSipHeaders, IdSipMessage;

type
  TIdSipMockCore = class(TIdSipAbstractCore)
  private
    fHandleRequestCalled: Boolean;
    fHandleResponseCalled: Boolean;
  public
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure HandleRequest(const Request: TIdSipRequest); override;
    procedure HandleResponse(const Response: TIdSipResponse); override;

    procedure Reset;

    property HandleRequestCalled:  Boolean read fHandleRequestCalled;
    property HandleResponseCalled: Boolean read fHandleResponseCalled;
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

procedure TIdSipMockCore.HandleRequest(const Request: TIdSipRequest);
begin
  fHandleRequestCalled := true;
end;

procedure TIdSipMockCore.HandleResponse(const Response: TIdSipResponse);
begin
  fHandleResponseCalled := true;
end;

procedure TIdSipMockCore.Reset;
begin
  fHandleRequestCalled  := true;
  fHandleResponseCalled := true;
end;

end.
