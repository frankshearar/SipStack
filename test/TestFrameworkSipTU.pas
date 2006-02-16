{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkSipTU;

interface

uses
  TestFrameworkSip, IdSipCore, IdSipMessage, SysUtils;

type
  TestTIdSipAction = class(TTestCaseTU,
                           IIdSipActionListener)
  protected
    ActionFailed:             Boolean;
    ActionParam:              TIdSipAction;
    AuthenticationChallenged: Boolean;
    FailReason:               String;
    Password:                 String;

    function  AddFailReason(const Msg: String): String;
    procedure CheckAuthentication(const AuthenticationHeaderName: String;
                                  const AuthorizationHeaderName: String;
                                  const QopType: String);
    procedure CheckAuthenticationReattempt(InitialAttempt,
                                           ReAttempt: TIdSipRequest;
                                           const AuthenticationHeaderName: String;
                                           const AuthorizationHeaderName: String;
                                           const MsgPrefix: String);
    function  CreateAction: TIdSipAction; virtual;
    function  CreateAuthorization(Challenge: TIdSipResponse): TIdSipAuthorizationHeader;
    function  IsInboundTest: Boolean;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure ReceiveBadExtensionResponse;
    procedure ReceiveBusyHere(Invite: TIdSipRequest);
    procedure ReceiveMovedTemporarily(Invite: TIdSipRequest;
                                      const Contacts: array of String); overload;
    procedure ReceiveMovedTemporarily(const Contact: String); overload;
    procedure ReceiveMovedTemporarily(const Contacts: array of String); overload;
    procedure ReceiveOkWithBody(Invite: TIdSipRequest;
                                const Body: String;
                                const ContentType: String);
    procedure ReceiveServiceUnavailable(Invite: TIdSipRequest);
  public
    procedure SetUp; override;

    procedure CheckAckSent(const Msg: String); override;
    procedure CheckRequestSent(const Msg: String); override;
    procedure CheckResponseSent(const Msg: String); override;
  published
    procedure TestAbandonAuthentication; virtual;
    procedure TestAuthentication;
    procedure TestAuthenticationChallenge;
    procedure TestIsInbound; virtual;
    procedure TestIsInvite; virtual;
    procedure TestIsOptions; virtual;
    procedure TestIsOwned; virtual;
    procedure TestIsRegistration; virtual;
    procedure TestIsSession; virtual;
    procedure TestLocalGruu; virtual;
    procedure TestMultipleAuthentication;
    procedure TestNetworkFailureTerminatesAction; virtual;
    procedure TestResend;
    procedure TestResendBeforeSend;
    procedure TestResendWithProxyAuth;
{
    procedure TestReceiveResponseBadExtension; // Currently our stack can't sent Requires; ergo we can't test in the usual fashion
    procedure TestReceiveResponseBadExtensionWithoutRequires;
}
  end;

  EInboundTest = class(Exception);

implementation

uses
  IdException, IdSipAuthentication;

//******************************************************************************
//* TestTIdSipAction                                                           *
//******************************************************************************
//* TestTIdSipAction Public methods ********************************************

procedure TestTIdSipAction.SetUp;
begin
  inherited SetUp;

  Self.ActionFailed             := false;
  Self.AuthenticationChallenged := false;
  Self.Password                 := 'mycotoxin';
end;

procedure TestTIdSipAction.CheckAckSent(const Msg: String);
begin
  inherited CheckAckSent(Self.AddFailReason(Msg));
end;

procedure TestTIdSipAction.CheckRequestSent(const Msg: String);
begin
  inherited CheckRequestSent(Self.AddFailReason(Msg));
end;

procedure TestTIdSipAction.CheckResponseSent(const Msg: String);
begin
  inherited CheckResponseSent(Self.AddFailReason(Msg));
end;

//* TestTIdSipAction Protected methods *****************************************

function TestTIdSipAction.AddFailReason(const Msg: String): String;
begin
  Result := Msg;
  if (Self.FailReason <> '') then
    Result := Result + '(' + Self.FailReason + ')';
end;

procedure TestTIdSipAction.CheckAuthentication(const AuthenticationHeaderName: String;
                                               const AuthorizationHeaderName: String;
                                               const QopType: String);
var
  Action:         TIdSipAction;
  AuthCreds:      TIdSipAuthorizationHeader;
  InitialRequest: TIdSipRequest;
  ReAttempt:      TIdSipRequest;
begin
  // This check/test is only valid for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;

  InitialRequest := Action.InitialRequest.Copy as TIdSipRequest;
  try
    Self.ReceiveUnauthorized(AuthenticationHeaderName, QopType);

    Self.MarkSentRequestCount;

    AuthCreds := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
    try
      Action.Resend(AuthCreds);
      CheckRequestSent(Self.ClassName + ': qop=' + QopType + ': no re-issue of '
                     + InitialRequest.Method + ' request');

      ReAttempt := Self.LastSentRequest;

      Self.CheckAuthenticationReattempt(InitialRequest,
                                        ReAttempt,
                                        AuthenticationHeaderName,
                                        AuthorizationHeaderName,
                                        Self.ClassName + ': qop=' + QopType + ':');
    finally
      AuthCreds.Free;
    end;
  finally
    InitialRequest.Free;
  end;
end;

procedure TestTIdSipAction.CheckAuthenticationReattempt(InitialAttempt,
                                                        ReAttempt: TIdSipRequest;
                                                        const AuthenticationHeaderName: String;
                                                        const AuthorizationHeaderName: String;
                                                        const MsgPrefix: String);
var
  A1:               String;
  A2:               String;
  Algo:             TIdAlgorithmFunction;
  Auth:             TIdSipAuthorizationHeader;
  Challenge:        TIdSipAuthenticateHeader;
  Digest:           TIdRequestDigestFunction;
  ExpectedResponse: String;
  Qop:              TIdQopFunction;
begin
  Challenge := Self.Dispatcher.Transport.LastResponse.FirstHeader(AuthenticationHeaderName) as TIdSipAuthenticateHeader;
  Auth      := ReAttempt.FirstHeader(AuthorizationHeaderName) as TIdSipAuthorizationHeader;

  CheckNotEquals(InitialAttempt.LastHop.Branch,
                 ReAttempt.LastHop.Branch,
                 'The new transaction used the old transaction''s branch');

  CheckEquals(InitialAttempt.CSeq.SequenceNo + 1,
              ReAttempt.CSeq.SequenceNo,
              MsgPrefix + ' Re-' + InitialAttempt.Method + ' CSeq sequence number');
  CheckEquals(InitialAttempt.Method,
              ReAttempt.Method,
              MsgPrefix + ' Method of new attempt');
  CheckEquals(InitialAttempt.RequestUri.Uri,
              ReAttempt.RequestUri.Uri,
              Self.ClassName + ': Re-' + InitialAttempt.Method + ' Request-URI');
  Check(ReAttempt.HasHeader(AuthorizationHeaderName),
        MsgPrefix + ' No ' + AuthorizationHeaderName + ' header in re-' + InitialAttempt.Method);

  CheckEquals(Challenge.AuthorizationScheme,
              Auth.AuthorizationScheme,
              MsgPrefix + ' No authorization scheme set');

  CheckEquals(Challenge.Nonce,
              Auth.Nonce,
              MsgPrefix + ' Nonce');

  CheckEquals(Challenge.Realm,
              Auth.Realm,
              MsgPrefix + ' Realm');

  CheckEquals(ReAttempt.RequestUri.AsString,
              Auth.DigestUri,
              MsgPrefix + ' URI');

  CheckEquals(Self.Core.Username,
              Auth.Username,
              MsgPrefix + ' Username');

  Algo   := A1For(Auth.Algorithm);
  Digest := RequestDigestFor(Auth.Qop);
  Qop    := A2For(Auth.Qop);

  A1 := Algo(Auth, Self.Password);
  A2 := Qop(Auth, ReAttempt.Method, ReAttempt.Body);

  ExpectedResponse := Digest(A1, A2, Auth);

  CheckEquals(ExpectedResponse,
              Auth.Response,
              MsgPrefix + ' Response');
end;

function TestTIdSipAction.CreateAction: TIdSipAction;
begin
  raise EInboundTest.Create(Self.ClassName
                       + ': Don''t call CreateAction on an inbound Action');
end;

function TestTIdSipAction.CreateAuthorization(Challenge: TIdSipResponse): TIdSipAuthorizationHeader;
var
  ChallengeHeader: TIdSipAuthenticateHeader;
  RealmInfo:       TIdRealmInfo;
begin
  ChallengeHeader := Challenge.AuthenticateHeader;

  if not Assigned(ChallengeHeader) then
    Fail(Self.ClassName + ': Missing a (Proxy|WWW)-Authenticate header');

  Self.Core.Keyring.AddKey(ChallengeHeader,
                           Self.LastSentRequest.RequestUri.AsString,
                           Self.Core.Username);

  RealmInfo := Self.Core.Keyring.Find(ChallengeHeader.Realm,
                                      Self.LastSentRequest.RequestUri.AsString);

  Result := RealmInfo.CreateAuthorization(Challenge,
                                          Self.LastSentRequest.Method,
                                          Self.LastSentRequest.Body,
                                          Self.Password);
end;

function TestTIdSipAction.IsInboundTest: Boolean;
var
  Action: TIdSipAction;
begin
  Result := true;

  try
    Action := Self.CreateAction;
    Result := Action.IsInbound;
  except
    on EInboundTest do;
  end;
end;

procedure TestTIdSipAction.OnAuthenticationChallenge(Action: TIdSipAction;
                                                     Response: TIdSipResponse);
begin
  Self.AuthenticationChallenged := true;
end;

procedure TestTIdSipAction.OnNetworkFailure(Action: TIdSipAction;
                                            ErrorCode: Cardinal;
                                            const Reason: String);
begin
  Self.FailReason  := Reason;
  Self.ActionParam := Action;
end;

procedure TestTIdSipAction.ReceiveBadExtensionResponse;
begin
  Self.ReceiveResponse(SIPBadExtension);
end;

procedure TestTIdSipAction.ReceiveBusyHere(Invite: TIdSipRequest);
begin
  Self.ReceiveResponse(Invite, SIPBusyHere);
end;

procedure TestTIdSipAction.ReceiveMovedTemporarily(Invite: TIdSipRequest;
                                                   const Contacts: array of String);
var
  I:        Integer;
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Invite,
                                          SIPMovedTemporarily);
  try
    for I := Low(Contacts) to High(Contacts) do
      Response.AddHeader(ContactHeaderFull).Value := Contacts[I];

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipAction.ReceiveMovedTemporarily(const Contact: String);
begin
  Self.ReceiveMovedTemporarily(Self.LastSentRequest, [Contact]);
end;

procedure TestTIdSipAction.ReceiveMovedTemporarily(const Contacts: array of String);
begin
  Self.ReceiveMovedTemporarily(Self.LastSentRequest, Contacts);
end;

procedure TestTIdSipAction.ReceiveOkWithBody(Invite: TIdSipRequest;
                                             const Body: String;
                                             const ContentType: String);
var
  Ok: TIdSipResponse;
begin
  Ok := Self.CreateRemoteOk(Invite);
  try
    Ok.Body                        := Body;
    Ok.ContentDisposition.Handling := DispositionSession;
    Ok.ContentLength               := Length(Body);
    Ok.ContentType                 := ContentType;

    Self.ReceiveResponse(Ok);
  finally
    Ok.Free;
  end;
end;

procedure TestTIdSipAction.ReceiveServiceUnavailable(Invite: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := TIdSipResponse.InResponseTo(Invite,
                                          SIPServiceUnavailable);
  try
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

//* TestTIdSipAction Published methods *****************************************

procedure TestTIdSipAction.TestAbandonAuthentication;
var
  Action: TIdSipAction;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Action.Terminate;
  Check(Action.IsTerminated,
        Self.ClassName + ': Action not terminated');
end;

procedure TestTIdSipAction.TestAuthentication;
begin
  Self.CheckAuthentication(WWWAuthenticateHeader, AuthorizationHeader, '');
end;

procedure TestTIdSipAction.TestAuthenticationChallenge;
var
  Action:    TIdSipAction;
  AuthCreds: TIdSipAuthorizationHeader;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Check(Self.AuthenticationChallenged,
        Action.ClassName + ' didn''t notify listeners of authentication challenge');

  Self.MarkSentRequestCount;

  AuthCreds := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
  try
    Action.Resend(AuthCreds);
  finally
    AuthCreds.Free;
  end;

  CheckRequestSent(Action.ClassName + ': No resend of request sent');
end;

procedure TestTIdSipAction.TestIsInbound;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsInbound,
        Action.ClassName + ' marked as an inbound action');
end;

procedure TestTIdSipAction.TestIsInvite;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsInvite,
        Action.ClassName + ' marked as an Invite');
end;

procedure TestTIdSipAction.TestIsOptions;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsOptions,
        Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipAction.TestIsOwned;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsOwned,
        Action.ClassName + ' marked as being owned');
end;

procedure TestTIdSipAction.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsRegistration,
        Action.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipAction.TestIsSession;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(not Action.IsSession,
        Action.ClassName + ' marked as a Session');
end;

procedure TestTIdSipAction.TestLocalGruu;
var
  Action: TIdSipAction;
  Gruu:   TIdSipContactHeader;
begin
  // Really, we only need ONE subclass to run this test as we're just checking
  // that a property sets correctly. The "not Self.IsInboundTest" doesn't mean
  // anything special - it's just that most inbound tests don't (can't) call
  // CreateAction.

  if not Self.IsInboundTest then begin
    // Self.UA owns the action!
    Action := Self.CreateAction;

    Gruu := TIdSipContactHeader.Create;
    try
      Gruu.Value := 'sip:case@fried-neurons.org;opaque=foo';

      Action.LocalGruu := Gruu;
      CheckEquals(Gruu.AsString,
                  Action.LocalGruu.AsString,
                  'LocalGruu property not assigned to');
    finally
      Gruu.Free;
    end;
  end;
end;

procedure TestTIdSipAction.TestMultipleAuthentication;
var
  Action:    TIdSipAction;
  ProxyAuth: TIdSipAuthorizationHeader;
  UAAuth:    TIdSipAuthorizationHeader;
begin
  //  ---                 OPTIONS                 ---> (for instance)
  // <---    407 Proxy Authentication Required    ---
  //  ---    OPTIONS (with proxy credentials)     --->
  // <---  401 Unauthorized (from the remote UA)  ---
  //  --- OPTIONS (with proxy and UA credentials) --->
  // <---                 200 OK                  ---

  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');

  ProxyAuth := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
  try
    Action.Resend(ProxyAuth);
    Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

    UAAuth := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
    try
      Self.MarkSentRequestCount;
      Action.Resend(UAAuth);
      CheckRequestSent('No request resent');
      Check(Self.LastSentRequest.HasProxyAuthorizationFor(ProxyAuth.Realm),
            'Missing proxy credentials');
      Check(Self.LastSentRequest.HasAuthorizationFor(UAAuth.Realm),
            'Missing UA credentials');
    finally
      UAAuth.Free;
    end;
  finally
    ProxyAuth.Free;
  end;
end;

procedure TestTIdSipAction.TestNetworkFailureTerminatesAction;
var
  Action: TIdSipAction;
begin
  if Self.IsInboundTest then Exit;

  Self.Dispatcher.Transport.FailWith := EIdConnectTimeout;
  Action := Self.CreateAction;
  Check(Action.IsTerminated,
        Action.ClassName + ' didn''t terminate after a network failure');
end;

procedure TestTIdSipAction.TestResend;
var
  Action:    TIdSipAction;
  AuthCreds: TIdSipAuthorizationHeader;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;

  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');
  AuthCreds := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
  try
    Self.MarkSentRequestCount;
    Action.Resend(AuthCreds);
    CheckRequestSent(Self.ClassName + ': Resend didn''t send a request');

    Check(Self.LastSentRequest.HasHeader(AuthCreds.Name),
          Self.ClassName + ': Resent request has no ' + AuthCreds.Name + ' header');
    Check(Self.LastSentRequest.AuthorizationFor(AuthCreds.Realm).Equals(AuthCreds),
          Self.ClassName + ': Incorrect Authorization header');

    Check(Action.InitialRequest.HasAuthorizationFor(AuthCreds.Realm),
          'Action''s InitialRequest has no record of the authentication');      
  finally
    AuthCreds.Free;
  end;
end;

procedure TestTIdSipAction.TestResendBeforeSend;
var
  Action:          TIdSipAction;
  AuthCreds:       TIdSipAuthorizationHeader;
  ThrowawayAction: TIdSipAction;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  // This looks very strange: We create an Action using the polymorphic
  // CreateAction. No surprise there. But CreateAction calls Send() on its
  // result, and for this test we don't want that. So we instantiate another
  // Action using the class type of the result of CreateAction.
  ThrowawayAction := Self.CreateAction;

  // Normally we wouldn't bother with receiving a response, but it allows us
  // to call CreateAuthorization later, without messing with junk.
  Self.ReceiveUnauthorized(WWWAuthenticateHeader, '');

  Action := Self.Core.AddOutboundAction(TIdSipActionClass(ThrowawayAction.ClassType));

  AuthCreds := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
  try
    Action.Resend(AuthCreds);
    Fail('Failed to reject attempt to Resend(AuthCreds) without having first '
       + 'invoked Send');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipAction.TestResendWithProxyAuth;
var
  Action:    TIdSipAction;
  ProxyAuth: TIdSipAuthorizationHeader;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Action := Self.CreateAction;
  Self.ReceiveUnauthorized(ProxyAuthenticateHeader, '');

  ProxyAuth := Self.CreateAuthorization(Self.Dispatcher.Transport.LastResponse);
  try
    Self.MarkSentRequestCount;

    Action.Resend(ProxyAuth);
    CheckRequestSent('Resend didn''t send a request');

    Check(Self.LastSentRequest.HasHeader(ProxyAuth.Name),
          'Resent request has no ' + ProxyAuth.Name + ' header');
    Check(Self.LastSentRequest.ProxyAuthorizationFor(ProxyAuth.Realm).Equals(ProxyAuth),
          'Incorrect Proxy-Authorization header');
  finally
    ProxyAuth.Free;
  end;
end;
{
procedure TestTIdSipAction.TestReceiveResponseBadExtension;
var
  Action:          TIdSipAction;
  ActionClassname: String;
begin
  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;
  Self.ReceiveBadExtensionResponse;

  Self.MarkSentRequestCount;

  CheckRequestSent(ActionClassname + ' request wasn''t reissued');
  Check(not Self.LastSentRequest.HasHeader(RequireHeader),
        'Require header still in 2nd attempt');
end;

procedure TestTIdSipAction.TestReceiveResponseBadExtensionWithoutRequires;
var
  Action:          TIdSipAction;
  ActionClassname: String;
begin

  // If we send a request that has no Requires header, but get a 420 Bad
  // Extension back (which can only come from a bad SIP implementation on the
  // remote end), then we must report a failure.

  // CreateAction creates an Action owned by Self.Core. When we free Self.Core
  // then it'll free Action.
  Action          := Self.CreateAction;
  ActionClassname := Action.ClassName;

  Self.ReceiveBadExtensionResponse;
  Check(Self.ActionFailed, ActionClassName + ' failure not reported');
end;
}

end.
