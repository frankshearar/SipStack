unit IdSipCore;

interface

uses
  IdSipDialog, IdSipHeaders, IdSipMessage, IdSipSession, IdSipTransaction, IdUri;

type
  TIdSipSessionEvent = procedure(const Self: TObject; const Session: TIdSipSession);

  TIdSipAbstractCore = class(TObject)
  private
    fDispatcher:   TIdSipTransactionDispatcher;
    fOnNewSession: TIdSipSessionEvent;

    procedure DoOnUnhandledRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnUnhandledResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure SetDispatcher(const Value: TIdSipTransactionDispatcher);
  protected
    procedure DoOnNewSession(const Session: TIdSipSession);  
  public
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; virtual; abstract;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); virtual; abstract;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); virtual; abstract;
    function  NextCallID: String;

    property Dispatcher:   TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property OnNewSession: TIdSipSessionEvent          read fOnNewSession write fOnNewSession;
  end;

  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    fContact: TIdSipContactHeader;
    fFrom:    TIdSipFromHeader;

    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure SetContact(const Value: TIdSipContactHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);
  public
    destructor Destroy; override;

    procedure Call(const Dest: TIdSipToHeader);
    function  CreateInvite(const Dest: TIdSipToHeader): TIdSipRequest;
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); override;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); override;
    function  NextTag: String;

    property Contact: TIdSipContactHeader read GetContact write SetContact;
    property From:    TIdSipFromHeader    read GetFrom write SetFrom;
  end;

  TIdSipMockCore = class(TIdSipAbstractCore)
  private
    fHandleUnmatchedRequestCalled: Boolean;
    fHandleUnmatchedResponseCalled: Boolean;
  public
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure HandleUnmatchedRequest(const Request: TIdSipRequest); override;
    procedure HandleUnmatchedResponse(const Response: TIdSipResponse); override;

    procedure Reset;

    property HandleUnmatchedRequestCalled:  Boolean read fHandleUnmatchedRequestCalled;
    property HandleUnmatchedResponseCalled: Boolean read fHandleUnmatchedResponseCalled;
  end;

implementation

uses
  IdSipConsts, IdSipRandom, IdStack, SysUtils;

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

function TIdSipAbstractCore.NextCallID: String;
var
  HostName: String;
begin
  // TODO: it would be good to get the name of the machine. Maybe the proxy,
  // seeing as this is a user agent.

  // Warning: GStack may not be initialised - it's lazily created.
  if Assigned(GStack) then
    HostName := GStack.LocalAddress
  else
    HostName := 'localhost';

  Result := IntToHex(TIdSipRandomNumber.Next, 8) + '@' + HostName;
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.DoOnNewSession(const Session: TIdSipSession);
begin
  if Assigned(Self.OnNewSession) then
    Self.OnNewSession(Self, Session);
end;

//* TIdSipAbstractCore Private methods *****************************************

procedure TIdSipAbstractCore.DoOnUnhandledRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.HandleUnmatchedRequest(Request);
end;

procedure TIdSipAbstractCore.DoOnUnhandledResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.HandleUnmatchedResponse(Response);
end;

procedure TIdSipAbstractCore.SetDispatcher(const Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;
  fDispatcher.OnUnhandledRequest  := Self.DoOnUnhandledRequest;
  fDispatcher.OnUnhandledResponse := Self.DoOnUnhandledResponse;
end;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.Contact.Free;
  Self.From.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.Call(const Dest: TIdSipToHeader);
var
  Invite: TIdSipRequest;
begin
  Invite := Self.CreateInvite(Dest);
  try
    Self.Dispatcher.SendRequest(Invite);
  finally
    Invite.Free;
  end;
end;

function TIdSipUserAgentCore.CreateInvite(const Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := CreateRequest(Dest);
  Result.Method := MethodInvite;
end;

function TIdSipUserAgentCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := Dest.Address.GetFullURI;

    if (Dest.Address.Protocol = SipsScheme) then
      Self.Contact.Address.Protocol := SipsScheme;

    Result.Headers.Add(Self.Contact);
    Result.CallID   := Self.NextCallID;
    Result.From     := Self.From;
    Result.From.Tag := Self.NextTag;
    Result.ToHeader := Dest;
  except
    Result.Free;

    raise;
  end;
end;

function TIdSipUserAgentCore.CreateResponse(const Request:      TIdSipRequest;
                                            const ResponseCode: Cardinal): TIdSipResponse;
var
  FirstRR:         TIdSipRecordRouteHeader;
  ReqRecordRoutes: TIdSipHeadersFilter;
begin
  Result := TIdSipResponse.Create;
  try
    Result.StatusCode := ResponseCode;

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
      Result.Headers.Add(Self.From);
    finally
      ReqRecordRoutes.Free;
    end;
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipUserAgentCore.HandleUnmatchedRequest(const Request: TIdSipRequest);
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

procedure TIdSipUserAgentCore.HandleUnmatchedResponse(const Response: TIdSipResponse);
begin
  // User Agents drop unmatched responses on the floor.
end;

function TIdSipUserAgentCore.NextTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := IntToHex(TIdSipRandomNumber.Next, 8)
          + IntToHex(TIdSipRandomNumber.Next, 8);
end;

//* TIdSipUserAgentCore Private methods ****************************************

function TIdSipUserAgentCore.GetContact: TIdSipContactHeader;
begin
  if not Assigned(fContact) then
    fContact := TIdSipContactHeader.Create;

  Result := fContact;
end;

function TIdSipUserAgentCore.GetFrom: TIdSipFromHeader;
begin
  if not Assigned(fFrom) then
    fFrom := TIdSipFromHeader.Create;

  Result := fFrom;
end;

procedure TIdSipUserAgentCore.SetContact(const Value: TIdSipContactHeader);
begin
  Assert(not Value.IsWildCard,
         'A wildcard Contact header may not be used here');

  Assert((Value.Address.Protocol = SipScheme) or (Value.Address.Protocol = SipsScheme),
         'Only SIP or SIPS URIs may be used.');

  Self.Contact.Assign(Value);
end;

procedure TIdSipUserAgentCore.SetFrom(const Value: TIdSipFromHeader);
begin
  Assert((Value.Address.Protocol = SipScheme) or (Value.Address.Protocol = SipsScheme),
         'Only SIP or SIPS URIs may be used.');

  Self.From.Assign(Value);
end;

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

function TIdSipMockCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := nil;
end;

function TIdSipMockCore.CreateResponse(const Request:      TIdSipRequest;
                                       const ResponseCode: Cardinal): TIdSipResponse;
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
