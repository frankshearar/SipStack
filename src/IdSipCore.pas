unit IdSipCore;

interface

uses
  Classes, Contnrs, IdSipDialog, IdSipHeaders, IdSipMessage, IdSipTransaction,
  IdUri;

type
  TIdSipAbstractCore = class(TObject)
  private
    fDispatcher:   TIdSipTransactionDispatcher;
    fHostName:     String;
    fSessions:     TObjectList;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure SetDispatcher(const Value: TIdSipTransactionDispatcher);
  protected
    procedure DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog);

    property Sessions: TObjectList read fSessions;
  public
    constructor Create; virtual;

    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; virtual; abstract;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    procedure HandleRequest(const Request: TIdSipRequest); virtual; abstract;
    procedure HandleResponse(const Response: TIdSipResponse); virtual; abstract;
    function  NextCallID: String;
    function  SessionCount: Cardinal;

    property Dispatcher:    TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:      String                      read fHostName write fHostName;
  end;

  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    fContact:       TIdSipContactHeader;
    fFrom:          TIdSipFromHeader;
    fLastBranch:    Cardinal;
    fUserAgentName: String;

    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure IncLastBranch;
    procedure ResetLastBranch;
    procedure SetContact(const Value: TIdSipContactHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);

    property LastBranch: Cardinal read fLastBranch;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Call(const Dest: TIdSipToHeader);
    function  CreateInvite(const Dest: TIdSipToHeader): TIdSipRequest;
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure HandleRequest(const Request: TIdSipRequest); override;
    procedure HandleResponse(const Response: TIdSipResponse); override;
    function  NextBranch: String;
    function  NextTag: String;

    property Contact:       TIdSipContactHeader read GetContact write SetContact;
    property From:          TIdSipFromHeader    read GetFrom write SetFrom;
    property UserAgentName: String              read fUserAgentName write fUserAgentName;
  end;

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

  TIdSipSession = class(TPersistent)
  private
    fDialogs: TIdSipDialogs;

  public
    constructor Create(const Dialog: TIdSipDialog);
    destructor  Destroy; override;

    procedure Cancel;
    procedure Modify;
    procedure Terminate;

    property Dialogs: TIdSipDialogs read fDialogs;
  end;

implementation

uses
  IdSipConsts, IdSipRandom, IdStack, SysUtils;

//******************************************************************************
//* TIdSipAbstractCore                                                         *
//******************************************************************************
//* TIdSipAbstractCore Public methods ******************************************

constructor TIdSipAbstractCore.Create;
begin
end;

function TIdSipAbstractCore.NextCallID: String;
begin
  Result := IntToHex(TIdSipRandomNumber.Next, 8) + '@' + Self.HostName;
end;

function TIdSipAbstractCore.SessionCount: Cardinal;
begin
  Result := Self.Sessions.Count;
end;

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog);
begin
  Self.Sessions.Add(TIdSipSession.Create(Dialog));
end;

//* TIdSipAbstractCore Private methods *****************************************

procedure TIdSipAbstractCore.DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.HandleRequest(Request);
end;

procedure TIdSipAbstractCore.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.HandleResponse(Response);
end;

procedure TIdSipAbstractCore.SetDispatcher(const Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;
  fDispatcher.OnNewDialog         := Self.DoOnNewDialog;
  fDispatcher.OnUnhandledRequest  := Self.DoOnReceiveRequest;
  fDispatcher.OnUnhandledResponse := Self.DoOnReceiveResponse;
end;

//******************************************************************************
//* TIdSipUserAgentCore                                                        *
//******************************************************************************
//* TIdSipUserAgentCore Public methods *****************************************

constructor TIdSipUserAgentCore.Create;
begin
  inherited Create;

  Self.ResetLastBranch;
  Self.fSessions := TObjectList.Create(true);
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.Sessions.Free;
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

    Result.AddHeader(Self.Contact);
    Result.CallID   := Self.NextCallID;
    Result.From     := Self.From;
    Result.From.Tag := Self.NextTag;
    Result.ToHeader := Dest;

    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/TCP localhost';
    Result.LastHop.Branch := Self.NextBranch;

    if (Self.UserAgentName <> '') then
      Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
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
      Result.AddHeaders(ReqRecordRoutes);

      if (ReqRecordRoutes.Count > 0) then begin
        FirstRR := ReqRecordRoutes.Items[0] as TIdSipRecordRouteHeader;
        if (FirstRR.Address.Protocol = SipsScheme) then
          Self.Contact.Address.Protocol := SipsScheme;
      end;

      if (Copy(Request.RequestUri, 1, 5) = SipsScheme + ':') then
        Self.Contact.Address.Protocol := SipsScheme;

      Result.AddHeader(Self.Contact);
      Result.AddHeader(Self.From);

      if (Self.UserAgentName <> '') then
        Result.AddHeader(UserAgentHeader).Value := Self.UserAgentName;
    finally
      ReqRecordRoutes.Free;
    end;
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipUserAgentCore.HandleRequest(const Request: TIdSipRequest);
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

procedure TIdSipUserAgentCore.HandleResponse(const Response: TIdSipResponse);
begin
  // User Agents drop unmatched responses on the floor.

  // However, if we store our INVITEs we can use this to trigger the creation
  // of dialog and session objects! TODO
end;

function TIdSipUserAgentCore.NextBranch: String;
begin
  // TODO
  // This is a CRAP way to generate a branch.
  // cf. RFC 3261 section 8.1.1.7
  // While this (almost) satisfies the uniqueness constraint (the branch is
  // unique for the lifetime of the instantiation of the UA), it just
  // seems sensible to generate an unguessable branch.
  Result := BranchMagicCookie + IntToStr(Self.LastBranch);

  Self.IncLastBranch;
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

procedure TIdSipUserAgentCore.IncLastBranch;
begin
  Inc(Self.fLastBranch);
end;

procedure TIdSipUserAgentCore.ResetLastBranch;
begin
  Self.fLastBranch := 0;
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

//******************************************************************************
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

constructor TIdSipSession.Create(const Dialog: TIdSipDialog);
begin
  inherited Create;

  Self.fDialogs := TIdSipDialogs.Create;
  Self.Dialogs.Add(Dialog);
end;

destructor TIdSipSession.Destroy;
begin
  Self.Dialogs.Free;

  inherited Destroy;
end;

procedure TIdSipSession.Cancel;
begin
end;

procedure TIdSipSession.Modify;
begin
end;

procedure TIdSipSession.Terminate;
begin
end;

end.
