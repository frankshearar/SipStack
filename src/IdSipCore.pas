unit IdSipCore;

interface

uses
  Classes, Contnrs, IdSipDialog, IdException, IdSipHeaders, IdSipMessage,
  IdSipTransaction, IdUri, SyncObjs;

type
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(Sender: TObject; const Session: TIdSipSession) of object;

  TIdSipAbstractCore = class(TObject)
  private
    fDispatcher:   TIdSipTransactionDispatcher;
    fHostName:     String;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure SetDispatcher(const Value: TIdSipTransactionDispatcher);
  protected
    procedure DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog); virtual;
  public
    constructor Create; virtual;

    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; virtual; abstract;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    procedure HandleRequest(const Request: TIdSipRequest); virtual; abstract;
    procedure HandleResponse(const Response: TIdSipResponse); virtual; abstract;
    function  NextCallID: String;

    property Dispatcher:    TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:      String                      read fHostName write fHostName;
  end;

  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    BranchLock:           TCriticalSection;
    fAllowedLanguageList: TStrings;
    fAllowedMethodList:   TStrings;
    fAllowedSchemeList:   TStrings;
    fContact:             TIdSipContactHeader;
    fFrom:                TIdSipFromHeader;
    fLastBranch:          Cardinal;
    fOnInvite:            TIdSipRequestEvent;
    fOnNewSession:        TIdSipSessionEvent;
    fSessions:            TObjectList;
    fUserAgentName:       String;
    SessionLock:          TCriticalSection;

    procedure DoOnInvite(const Request: TIdSipRequest);
    procedure DoOnNewSession(const Session: TIdSipSession);
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure RejectBadRequest(const Request: TIdSipRequest; const Reason: String);
    procedure RejectRequestBadExtension(const Request: TIdSipRequest);
    procedure RejectRequestMethodNotAllowed(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(const Request: TIdSipRequest);
    procedure RemoveSession(const Session: TIdSipSession);
    procedure ResetLastBranch;
    procedure SendRinging(const Request: TIdSipRequest);
    procedure SetContact(const Value: TIdSipContactHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);

    property AllowedLanguageList: TStrings    read fAllowedLanguageList;
    property AllowedMethodList:   TStrings    read fAllowedMethodList;
    property AllowedSchemeList:   TStrings    read fAllowedSchemeList;
    property Sessions:            TObjectList read fSessions;
  protected
    procedure DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AcceptCall(const Request: TIdSipRequest);
    procedure AddAllowedLanguage(const LanguageID: String);
    procedure AddAllowedMethod(const Method: String);
    procedure AddAllowedScheme(const Scheme: String);
    function  AllowedLanguages: String;
    function  AllowedMethods: String;
    function  AllowedSchemes: String;
    procedure Call(const Dest: TIdSipToHeader);
    function  CreateInvite(const Dest: TIdSipToHeader): TIdSipRequest;
    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; override;
    function  CreateResponse(const Request:      TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; override;
    procedure HandleRequest(const Request: TIdSipRequest); override;
    procedure HandleResponse(const Response: TIdSipResponse); override;
    function  HasUnknownContentLanguage(const Request: TIdSipRequest): Boolean;
    function  HasUnknownContentEncoding(const Request: TIdSipRequest): Boolean;
    function  HasUnknownContentType(const Request: TIdSipRequest): Boolean;
    function  IsExtensionAllowed(const Extension: String): Boolean;
    function  IsMethodAllowed(const Method: String): Boolean;
    function  IsSchemeAllowed(const Scheme: String): Boolean;
    function  NextBranch: String;
    function  NextTag: String;
    procedure RejectRequest(const Request: TIdSipRequest; const Reason: Cardinal);
    function  SessionCount: Cardinal;

    property OnInvite:       TIdSipRequestEvent  read fOnInvite write fOnInvite;
    property OnNewSession:   TIdSipSessionEvent  read fOnNewSession write fOnNewSession;
    property Contact:        TIdSipContactHeader read GetContact write SetContact;
    property From:           TIdSipFromHeader    read GetFrom write SetFrom;
    property UserAgentName:  String              read fUserAgentName write fUserAgentName;
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
    fCore:    TIdSipAbstractCore;
    fDialogs: TIdSipDialogs;

    property Core: TIdSipAbstractCore read fCore;
  public
    constructor Create(const Dialog: TIdSipDialog; const UA: TIdSipAbstractCore);
    destructor  Destroy; override;

    procedure Cancel;
    procedure Modify;
    procedure Terminate;

    property Dialogs: TIdSipDialogs read fDialogs;
  end;

  EIdSipBadSyntax = class(EIdException);

const
  MissingContactHeader = 'Missing Contact Header';

implementation

uses
  IdGlobal, IdSipConsts, IdSipRandom, IdStack, SysUtils;

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

//* TIdSipAbstractCore Protected methods ***************************************

procedure TIdSipAbstractCore.DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog);
begin
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

  Self.BranchLock  := TCriticalSection.Create;
  Self.SessionLock := TCriticalSection.Create;

  Self.ResetLastBranch;
  Self.fAllowedLanguageList := TStringList.Create;
  Self.fAllowedMethodList := TStringList.Create;
  Self.fAllowedSchemeList := TStringList.Create;
  Self.fSessions := TObjectList.Create(true);

  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);

  Self.AddAllowedScheme(SipScheme);
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.Sessions.Free;
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;
  Self.AllowedLanguageList.Free;
  Self.Contact.Free;
  Self.From.Free;
  Self.SessionLock.Free;
  Self.BranchLock.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.AcceptCall(const Request: TIdSipRequest);
var
  ID:          TIdSipDialogID;
  NewDialog:   TIdSipDialog;
  Response:    TIdSipResponse;
  RouteFilter: TIdSipHeadersFilter;
  Session:     TIdSipSession;
begin
  Response := Self.CreateResponse(Request, SIPOK);
  try
    Response.ToHeader.Tag := Self.NextTag;

    RouteFilter := TIdSipHeadersFilter.Create(Request.Headers, RouteHeader);
    try
      ID := TIdSipDialogID.Create(Request.CallID, Response.ToHeader.Tag, Request.From.Tag);
      try
        NewDialog := TIdSipDialog.Create(ID,
                                         0,
                                         Request.CSeq.SequenceNo,
                                         Request.ToHeader.Address,
                                         Request.From.Address,
                                         (Request.FirstHeader(ContactHeaderFull) as TIdSipContactHeader).Address,
                                         Request.HasSipsUri,
                                         RouteFilter);
        try
          Session := TIdSipSession.Create(NewDialog, Self);
          try
            Self.Sessions.Add(Session);

            Self.Dispatcher.SendResponse(Response);
            Self.DoOnNewSession(Session);
          except
            Self.RemoveSession(Session);

            raise;
          end;
        finally
          NewDialog.Free;
        end;
      finally
        ID.Free;
      end;
    finally
      RouteFilter.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.AddAllowedLanguage(const LanguageID: String);
begin
  if (Trim(LanguageID) = '') then
    raise EIdSipBadSyntax.Create('Not a valid language identifier');

  if (Self.AllowedLanguageList.IndexOf(LanguageID) = -1) then
    Self.AllowedLanguageList.Add(LanguageID);
end;

procedure TIdSipUserAgentCore.AddAllowedMethod(const Method: String);
begin
  if not TIdSipParser.IsToken(Method) then
    raise EIdSipBadSyntax.Create('Not a token');

  if (Self.AllowedMethodList.IndexOf(Method) = -1) then
    Self.AllowedMethodList.Add(Method);
end;

procedure TIdSipUserAgentCore.AddAllowedScheme(const Scheme: String);
begin
  if not TIdSipParser.IsScheme(Scheme) then
    raise EIdSipBadSyntax.Create('Not a valid scheme');

  if (Self.AllowedSchemeList.IndexOf(Scheme) = -1) then
    Self.AllowedSchemeList.Add(Scheme);
end;

function TIdSipUserAgentCore.AllowedLanguages: String;
begin
  Result := Self.AllowedLanguageList.CommaText;
end;

function TIdSipUserAgentCore.AllowedMethods: String;
begin
  Result := Self.AllowedMethodList.CommaText;
end;

function TIdSipUserAgentCore.AllowedSchemes: String;
begin
  Result := Self.AllowedSchemeList.CommaText;
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
    Result.RequestUri := Dest.Address;

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

      if Request.HasSipsUri then
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
  Self.SendRinging(Request);

  // Section 8.2
  // inspect the method - 8.2.1
  if not Self.IsMethodAllowed(Request.Method) then begin
    Self.RejectRequestMethodNotAllowed(Request);
    Exit;
  end;

  // inspect the headers - 8.2.2

  // To & Request-URI - 8.2.2.1
  if not Self.IsSchemeAllowed(Request.RequestUri.Protocol) then begin
    Self.RejectRequest(Request, SIPUnsupportedURIScheme);
    Exit;
  end;

  // Merged requests - 8.2.2.2
  if not Request.ToHeader.HasTag and Self.Dispatcher.LoopDetected(Request) then begin
    Self.RejectRequest(Request, SIPLoopDetected);
    Exit;
  end;

  // Require - 8.2.2.3
  if Request.HasHeader(RequireHeader) then begin
    Self.RejectRequestBadExtension(Request);
    Exit;
  end;

  // Content processing - 8.2.3
  if Self.HasUnknownContentEncoding(Request) then begin
    Self.RejectRequestUnknownContentEncoding(Request);
    Exit;
  end;

  if Self.HasUnknownContentLanguage(Request) then begin
    Self.RejectRequestUnknownContentLanguage(Request);
    Exit;
  end;

  if Self.HasUnknownContentType(Request) then begin
    Self.RejectRequestUnknownContentType(Request);
    Exit;
  end;

  // Processing the request - 8.2.5
  if Request.IsInvite then begin
    // Section 8.1.1.8 says that a request that can start a dialog (like an
    // INVITE), MUST contain a Contact.
    if not Request.HasHeader(ContactHeaderFull) then
      Self.RejectBadRequest(Request, MissingContactHeader)
    else
      Self.DoOnInvite(Request);
  end
  else if Request.IsBye then
  else if Request.IsCancel then;

  // Generating the response - 8.2.6
end;

procedure TIdSipUserAgentCore.HandleResponse(const Response: TIdSipResponse);
begin
  // User Agents drop unmatched responses on the floor.

  // However, if we store our INVITEs we can use this to trigger the creation
  // of dialog and session objects! TODO
end;

function TIdSipUserAgentCore.HasUnknownContentLanguage(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentLanguageHeader)
       and (Self.AllowedLanguageList.IndexOf(Request.FirstHeader(ContentLanguageHeader).Value) = -1);
end;

function TIdSipUserAgentCore.HasUnknownContentEncoding(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentEncodingHeaderFull);
end;

function TIdSipUserAgentCore.HasUnknownContentType(const Request: TIdSipRequest): Boolean;
begin
  Result := Request.HasHeader(ContentTypeHeaderFull)
       and (Request.FirstHeader(ContentTypeHeaderFull).Value <> SdpMimeType);
end;

function TIdSipUserAgentCore.IsExtensionAllowed(const Extension: String): Boolean;
begin
  Result := false;
end;

function TIdSipUserAgentCore.IsMethodAllowed(const Method: String): Boolean;
begin
  Result := Self.AllowedMethodList.IndexOf(Method) >= 0;
end;

function TIdSipUserAgentCore.IsSchemeAllowed(const Scheme: String): Boolean;
begin
  Result := Self.AllowedSchemeList.IndexOf(Scheme) >= 0;
end;

function TIdSipUserAgentCore.NextBranch: String;
begin
  Self.BranchLock.Acquire;
  try
    // TODO
    // This is a CRAP way to generate a branch.
    // cf. RFC 3261 section 8.1.1.7
    // While this (almost) satisfies the uniqueness constraint (the branch is
    // unique for the lifetime of the instantiation of the UA), it just
    // seems sensible to generate an unguessable branch.
    Result := BranchMagicCookie + IntToStr(Self.fLastBranch);

    Inc(Self.fLastBranch);
  finally
    Self.BranchLock.Release;
  end;
end;

function TIdSipUserAgentCore.NextTag: String;
begin
  // TODO
  // This is a CRAP way to generate a tag.
  // cf. RFC 3261 section 19.3
  Result := IntToHex(TIdSipRandomNumber.Next, 8)
          + IntToHex(TIdSipRandomNumber.Next, 8);
end;

procedure TIdSipUserAgentCore.RejectRequest(const Request: TIdSipRequest; const Reason: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, Reason);
  try
    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

function TIdSipUserAgentCore.SessionCount: Cardinal;
begin
  Result := Self.Sessions.Count;
end;

//* TIdSipUserAgentCore Protected methods **************************************

procedure TIdSipUserAgentCore.DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog);
begin
  Self.Sessions.Add(TIdSipSession.Create(Dialog, Self));
end;

//* TIdSipUserAgentCore Private methods ****************************************

procedure TIdSipUserAgentCore.DoOnInvite(const Request: TIdSipRequest);
begin
  if Assigned(Self.OnInvite) then
    Self.OnInvite(Self, Request);
end;

procedure TIdSipUserAgentCore.DoOnNewSession(const Session: TIdSipSession);
begin
  if Assigned(Self.OnNewSession) then
    Self.OnNewSession(Self, Session);
end;

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

procedure TIdSipUserAgentCore.RejectBadRequest(const Request: TIdSipRequest;
                                               const Reason: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadRequest);
  try
    Response.StatusText := Reason;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectRequestBadExtension(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPBadExtension);
  try
    Response.AddHeader(UnsupportedHeader).Value := Request.FirstHeader(RequireHeader).Value;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectRequestMethodNotAllowed(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPMethodNotAllowed);
  try
    Response.AddHeader(AllowHeader).Value := Self.AllowedMethods;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectRequestUnknownContentEncoding(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptEncodingHeader).Value := '';

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectRequestUnknownContentLanguage(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptLanguageHeader).Value := Self.AllowedLanguages;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RejectRequestUnknownContentType(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPUnsupportedMediaType);
  try
    Response.AddHeader(AcceptHeader).Value := SdpMimeType;

    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TIdSipUserAgentCore.RemoveSession(const Session: TIdSipSession);
begin
  Self.Sessions.Remove(Session);
end;

procedure TIdSipUserAgentCore.ResetLastBranch;
begin
  Self.BranchLock.Acquire;
  try
    Self.fLastBranch := 0;
  finally
    Self.BranchLock.Release;
  end;
end;

procedure TIdSipUserAgentCore.SendRinging(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPRinging);
  try
    Self.Dispatcher.SendResponse(Response);
  finally
    Response.Free;
  end;
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

constructor TIdSipSession.Create(const Dialog: TIdSipDialog; const UA: TIdSipAbstractCore);
begin
  inherited Create;

  Self.fCore := UA;
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
//  Self.Core.Cancel(Self);
end;

procedure TIdSipSession.Modify;
begin
end;

procedure TIdSipSession.Terminate;
begin
end;

end.
