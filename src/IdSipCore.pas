unit IdSipCore;

// Some overarching principles followed in this implementation of a SIP/2.0
// (RFC 3261) stack:
// * The lifetime of all objects is manually managed. Objects that implement
//   interfaces are NOT reference counted.
// * Value Objects are used when possible.
// * If an object A receives some object B that it is expected to store as data
//   then A must store a COPY of B. Typical objects are: TIdURI, TIdSipDialogID,
//   TIdSipMessage.
// * Each layer is aware of the layers beneath it. We try to make each layer
//   aware of ONLY the layer immediately below it, but that's not always
//   possible.
// * Events or Listeners are used to propogate messages up the stack, and method
//   calls to propogate messages down the stack.
// * Typecasting is avoided as much as possible by using polymorphism and, in
//   certain situations where polymorphism can't cut it, the Visitor pattern.
// * TObjectLists always manage the lifetime of the objects they contain.

interface

uses
  Classes, Contnrs, IdSipDialog, IdException, IdSipHeaders, IdSipMessage,
  IdSipTransaction, IdUri, SyncObjs;

type
  TIdSipSession = class;
  TIdSipSessionEvent = procedure(Sender: TObject; const Session: TIdSipSession) of object;

  TIdSipAbstractCore = class(TObject)
  private
    fDispatcher: TIdSipTransactionDispatcher;
    fHostName:   String;
    fOnFail:     TIdSipFailEvent;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure DoOnTransactionFail(Sender: TObject; const Reason: String);
    procedure SetDispatcher(const Value: TIdSipTransactionDispatcher);
  protected
    procedure DoOnNewDialog(Sender: TObject; const Dialog: TIdSipDialog); virtual;
  public
    constructor Create; virtual;

    function  CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest; virtual; abstract;
    function  CreateResponse(const Request: TIdSipRequest;
                             const ResponseCode: Cardinal): TIdSipResponse; virtual; abstract;
    procedure HandleRequest(const Request: TIdSipRequest); virtual; abstract;
    procedure HandleResponse(const Response: TIdSipResponse); virtual; abstract;
    function  NextCallID: String;

    property Dispatcher: TIdSipTransactionDispatcher read fDispatcher write SetDispatcher;
    property HostName:   String                      read fHostName write fHostName;
    property OnFail:     TIdSipFailEvent             read fOnFail write fOnFail;
  end;

  // I am a User Agent. I (usually) represent a human being in the SIP network.
  // It is my responsibility to:
  // * inform any listeners when new sessions are established, modified or ended;
  // * allow my users to accept incoming "calls", make outgoing "calls"
  TIdSipUserAgentCore = class(TIdSipAbstractCore)
  private
    BranchLock:           TCriticalSection;
    DialogLock:           TCriticalSection;
    Dialogs:              TIdSipDialogs;
    fAllowedLanguageList: TStrings;
    fAllowedMethodList:   TStrings;
    fAllowedSchemeList:   TStrings;
    fContact:             TIdSipContactHeader;
    fFrom:                TIdSipFromHeader;
    fLastBranch:          Cardinal;
    fOnInvite:            TIdSipRequestEvent;
    fUserAgentName:       String;

    procedure DoOnInvite(const Request: TIdSipRequest);
    function  GetContact: TIdSipContactHeader;
    function  GetFrom: TIdSipFromHeader;
    procedure RejectBadRequest(const Request: TIdSipRequest; const Reason: String);
    procedure RejectRequestBadExtension(const Request: TIdSipRequest);
    procedure RejectRequestMethodNotAllowed(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentEncoding(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentLanguage(const Request: TIdSipRequest);
    procedure RejectRequestUnknownContentType(const Request: TIdSipRequest);
    procedure ResetLastBranch;
    procedure SendRinging(const Request: TIdSipRequest);
    procedure SetContact(const Value: TIdSipContactHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);

    property AllowedLanguageList: TStrings read fAllowedLanguageList;
    property AllowedMethodList:   TStrings read fAllowedMethodList;
    property AllowedSchemeList:   TStrings read fAllowedSchemeList;
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
    function  CreateResponse(const Request: TIdSipRequest;
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

    property OnInvite:       TIdSipRequestEvent  read fOnInvite write fOnInvite;
    property Contact:        TIdSipContactHeader read GetContact write SetContact;
    property From:           TIdSipFromHeader    read GetFrom write SetFrom;
    property UserAgentName:  String              read fUserAgentName write fUserAgentName;
  end;

  TIdSipSession = class(TPersistent)
  private
    fCore:   TIdSipUserAgentCore;
    fDialog: TIdSipDialog;

    property Core: TIdSipUserAgentCore read fCore;
  public
    constructor Create(const Dialog: TIdSipDialog;
                       const UA:     TIdSipUserAgentCore);
    destructor  Destroy; override;

    procedure Cancel;
    function  CreateBye: TIdSipRequest;
    procedure HangUp;
    procedure Modify;

    property Dialog: TIdSipDialog read fDialog;
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

procedure TIdSipAbstractCore.DoOnTransactionFail(Sender: TObject; const Reason: String);
begin
  if Assigned(Self.OnFail) then
    Self.OnFail(Self, Reason);
end;

procedure TIdSipAbstractCore.SetDispatcher(const Value: TIdSipTransactionDispatcher);
begin
  fDispatcher := Value;
  fDispatcher.OnTransactionFail   := Self.DoOnTransactionFail;
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
  Self.DialogLock  := TCriticalSection.Create;
  Self.Dialogs     := TIdSipDialogs.Create;

  Self.ResetLastBranch;
  Self.fAllowedLanguageList := TStringList.Create;
  Self.fAllowedMethodList := TStringList.Create;
  Self.fAllowedSchemeList := TStringList.Create;

  Self.AddAllowedMethod(MethodBye);
  Self.AddAllowedMethod(MethodCancel);
  Self.AddAllowedMethod(MethodInvite);

  Self.AddAllowedScheme(SipScheme);
end;

destructor TIdSipUserAgentCore.Destroy;
begin
  Self.AllowedSchemeList.Free;
  Self.AllowedMethodList.Free;
  Self.AllowedLanguageList.Free;
  Self.Contact.Free;
  Self.From.Free;
  Self.Dialogs.Free;
  Self.DialogLock.Free;
  Self.BranchLock.Free;

  inherited Destroy;
end;

procedure TIdSipUserAgentCore.AcceptCall(const Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateResponse(Request, SIPOK);
  try
    Response.ToHeader.Tag := Self.NextTag;

    Self.Dispatcher.SendToTransaction(Response);
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
    Self.Dispatcher.AddClientTransaction(Invite);
  finally
    Invite.Free;
  end;
end;

function TIdSipUserAgentCore.CreateInvite(const Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := CreateRequest(Dest);
  Result.Method := MethodInvite;

  Result.CSeq.Method := MethodInvite;
  Result.CSeq.SequenceNo := 0;
end;

function TIdSipUserAgentCore.CreateRequest(const Dest: TIdSipToHeader): TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Result.RequestUri := Dest.Address;

    if Dest.HasSipsUri then
      Self.Contact.Address.Protocol := SipsScheme;

    Result.AddHeader(Self.Contact);
    Result.CallID   := Self.NextCallID;
    Result.From     := Self.From;
    Result.From.Tag := Self.NextTag;
    Result.ToHeader := Dest;

    Result.AddHeader(ViaHeaderFull).Value := SipVersion + '/TCP localhost;branch=' + BranchMagicCookie;
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
  FirstRR:          TIdSipRecordRouteHeader;
  ReqRecordRoutes:  TIdSipHeadersFilter;
  TimestampHeaders: TIdSipHeadersFilter;
begin
  Result := TIdSipResponse.Create;
  try
    Result.StatusCode := ResponseCode;

    // cf RFC 3261 section 8.2.6.1
    if (Result.StatusCode = SIPTrying) then begin
      TimestampHeaders := TIdSipHeadersFilter.Create(Request.Headers,
                                                     TimestampHeader);
      try
        Result.AddHeaders(TimestampHeaders);
      finally
        TimestampHeaders.Free;
      end;
    end;

    // cf RFC 3261 section 8.2.6.2
    Result.CallID   := Request.CallID;
    Result.CSeq     := Request.CSeq;
    Result.From     := Request.From;
    Result.ToHeader := Request.ToHeader;
    Result.Path     := Request.Path;

    // cf RFC 3261 section 12.1.1
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
  Self.Dispatcher.AddServerTransaction(Request);

  // cf RFC 3261 section 8.2
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
    raise Exception.Create('Handling BYEs not implemented yet')
  else if Request.IsCancel then
    raise Exception.Create('Handling CANCELs not implemented yet');

  // Generating the response - 8.2.6
end;

procedure TIdSipUserAgentCore.HandleResponse(const Response: TIdSipResponse);
begin
  // User Agents drop unmatched responses on the floor.
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
    Self.Dispatcher.SendToTransaction(Response);
  finally
    Response.Free;
  end;
end;

//* TIdSipUserAgentCore Private methods ****************************************

procedure TIdSipUserAgentCore.DoOnInvite(const Request: TIdSipRequest);
begin
  Self.SendRinging(Request);

  if Assigned(Self.OnInvite) then
    Self.OnInvite(Self, Request);
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

    Self.Dispatcher.SendToTransaction(Response);
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

    Self.Dispatcher.SendToTransaction(Response);
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

    Self.Dispatcher.SendToTransaction(Response);
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

    Self.Dispatcher.SendToTransaction(Response);
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

    Self.Dispatcher.SendToTransaction(Response);
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

    Self.Dispatcher.SendToTransaction(Response);
  finally
    Response.Free;
  end;
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
    Self.Dispatcher.SendToTransaction(Response);
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
//* TIdSipSession                                                              *
//******************************************************************************
//* TIdSipSession Public methods ***********************************************

constructor TIdSipSession.Create(const Dialog: TIdSipDialog;
                                 const UA:     TIdSipUserAgentCore);
begin
  inherited Create;

  Self.fCore := UA;
  Self.fDialog := TIdSipDialog.Create(Dialog);
end;

destructor TIdSipSession.Destroy;
begin
  Self.Dialog.Free;

  inherited Destroy;
end;

procedure TIdSipSession.Cancel;
begin
//  Self.Core.Cancel(Self);
end;

function TIdSipSession.CreateBye: TIdSipRequest;
begin
  Result := Self.Dialog.CreateBye;
end;

procedure TIdSipSession.HangUp;
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateBye;
  try
    Self.Core.Dispatcher.SendToTransaction(Bye);
  finally
    Bye.Free;
  end;
end;

procedure TIdSipSession.Modify;
begin
end;

end.
