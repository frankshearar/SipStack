unit IdSipCore;

interface

uses
  IdSipHeaders, IdSipMessage, IdSipSession, IdUri;

type
  TIdSipSessionEvent = procedure(const Self: TObject; const Session: TIdSipSession);

  TIdSipAbstractCore = class(TObject)
  public
    function  CreateRequest(const RequestUri: TIdUri): TIdSipRequest; virtual; abstract;
    function  CreateResponse(const Request: TIdSipRequest): TIdSipResponse; virtual; abstract;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); virtual; abstract;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); virtual; abstract;
  end;

  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    fContact: TIdSipContactHeader;

    function  GetContact: TIdSipContactHeader;
    procedure SetContact(const Value: TIdSipContactHeader);
  public
    destructor Destroy; override;

    property Contact: TIdSipContactHeader read GetContact write SetContact;
  end;
  TIdSipUserAgentCoreClass = class of TIdSipUserAgentCore;

  TIdSipUserAgentClientCore = class(TIdSipUserAgentCore)
  public
    function  CreateRequest(const RequestUri: TIdUri): TIdSipRequest; override;
    function  CreateResponse(const Request: TIdSipRequest): TIdSipResponse; override;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); override;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); override;
  end;

  TIdSipUserAgentServerCore = class(TIdSipUserAgentCore)
  public
    function  CreateRequest(const RequestUri: TIdUri): TIdSipRequest; override;
    function  CreateResponse(const Request: TIdSipRequest): TIdSipResponse; override;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); override;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); override;
  end;

  TIdSipMockCore = class(TIdSipAbstractCore)
  private
    fHandleUnmatchedRequestCalled: Boolean;
    fHandleUnmatchedResponseCalled: Boolean;
  public
    function  CreateRequest(const RequestUri: TIdUri): TIdSipRequest; override;
    function  CreateResponse(const Request: TIdSipRequest): TIdSipResponse; override;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); override;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); override;

    procedure Reset;

    property HandleUnmatchedRequestCalled:  Boolean read fHandleUnmatchedRequestCalled;
    property HandleUnmatchedResponseCalled: Boolean read fHandleUnmatchedResponseCalled;
  end;

implementation

uses
  IdSipConsts;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.Contact.Free;

  inherited Destroy;
end;

//* TIdSipUserAgentCore Private methods ****************************************

function TIdSipUserAgentCore.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

procedure TIdSipUserAgentCore.SetContact(const Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'A wildcard Contact header may not be used here');

  Assert((Value.Address.Protocol = SipScheme) or (Value.Address.Protocol = SipsScheme),
         'Only SIP or SIPS URIs may be used.');

  Self.Contact.Assign(Value);
end;

//******************************************************************************
//* TIdSipUserAgentClientCore                                                  *
//******************************************************************************
//* TIdSipUserAgentClientCore Public methods ***********************************

function TIdSipUserAgentClientCore.CreateRequest(const RequestUri: TIdUri): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := RequestUri.GetFullURI;

    if (RequestUri.Protocol = SipsScheme) then
      Self.Contact.Address.Protocol := SipsScheme;

    Result.Headers.Add(Self.Contact);
  except
    Result.Free;

    raise;
  end;
end;

function TIdSipUserAgentClientCore.CreateResponse(const Request: TIdSipRequest): TIdSipResponse;
begin
  Result := nil;
end;

procedure TIdSipUserAgentClientCore.HandleUnmatchedRequest(const Request: TIdSipRequest);
begin
end;

procedure TIdSipUserAgentClientCore.HandleUnmatchedResponse(const Response: TIdSipResponse);
begin
end;

//******************************************************************************
//* TIdSipUserAgentServerCore                                                  *
//******************************************************************************
//* TIdSipUserAgentServerCore Public methods ***********************************

function TIdSipUserAgentServerCore.CreateRequest(const RequestUri: TIdUri): TIdSipRequest;
begin
  Result := nil;
end;

function TIdSipUserAgentServerCore.CreateResponse(const Request: TIdSipRequest): TIdSipResponse;
var
  FirstRR:         TIdSipRecordRouteHeader;
  ReqRecordRoutes: TIdSipHeadersFilter;
begin
  Result := TIdSipResponse.Create;
  try
    Result.CallID   := Request.CallID;
    Result.CSeq     := Request.CSeq;
    Result.From     := Request.From;
    Result.ToHeader := Request.ToHeader;
    Result.Path     := Request.Path;

    ReqRecordRoutes := TIdSipHeadersFilter.Create(Request.Headers, RecordRouteHeader);
    try
      Result.Headers.Add(ReqRecordRoutes);

      if (ReqRecordRoutes.Count > 0) then begin
        FirstRR := ReqRecordRoutes.Items[0] as TIdSipRecordRouteHeader;
        if (FirstRR.Address.Protocol = SipsScheme) then
          Self.Contact.Address.Protocol := SipsScheme;
      end;

      if (Copy(Request.RequestUri, 1, 5) = SipsScheme + ':') then
        Self.Contact.Address.Protocol := SipsScheme;

      Result.Headers.Add(Self.Contact);
    finally
      ReqRecordRoutes.Free;
    end;
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipUserAgentServerCore.HandleUnmatchedRequest(const Request: TIdSipRequest);
begin
  // Section 8.2
  // inspect the method - 8.2.1
  // inspect the headers - 8.2.2
  // To & Request-URI - 8.2.2.1
  // Merged requests - 8.2.2.2
  // Require - 8.2.2.3
  // Content processing - 8.2.3
  // Applying extensions - 8.2.4.
  // Processing the request - 8.2.5
  // Generating the response - 8.2.6
end;

procedure TIdSipUserAgentServerCore.HandleUnmatchedResponse(const Response: TIdSipResponse);
begin
end;

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

function TIdSipMockCore.CreateRequest(const RequestUri: TIdUri): TIdSipRequest;
begin
  Result := nil;
end;

function TIdSipMockCore.CreateResponse(const Request: TIdSipRequest): TIdSipResponse;
begin
  Result := nil;
end;

procedure TIdSipMockCore.HandleUnmatchedRequest(const Request: TIdSipRequest);
begin
  fHandleUnmatchedRequestCalled := true;
end;

procedure TIdSipMockCore.HandleUnmatchedResponse(const Response: TIdSipResponse);
begin
  fHandleUnmatchedResponseCalled := true;
end;

procedure TIdSipMockCore.Reset;
begin
  fHandleUnmatchedRequestCalled  := true;
  fHandleUnmatchedResponseCalled := true;
end;

end.
