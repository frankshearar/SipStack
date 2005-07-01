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
  TestFrameworkSip, IdSipCore, IdSipMessage;

type
  TestTIdSipAction = class(TTestCaseTU,
                           IIdSipActionListener)
  protected
    ActionFailed: Boolean;
    ActionParam:  TIdSipAction;
    FailReason:   String;

    function  CreateAction: TIdSipAction; virtual;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure ReceiveBadExtensionResponse;
    procedure ReceiveOkWithBody(Invite: TIdSipRequest;
                                const Body: String;
                                const ContentType: String);
    procedure ReceiveServiceUnavailable(Invite: TIdSipRequest);
  public
    procedure SetUp; override;
  published
    procedure TestIsInbound; virtual;
    procedure TestIsInvite; virtual;
    procedure TestIsOptions; virtual;
    procedure TestIsRegistration; virtual;
    procedure TestIsSession; virtual;
{
    procedure TestReceiveResponseBadExtension; // Currently our stack can't sent Requires; ergo we can't test in the usual fashion
    procedure TestReceiveResponseBadExtensionWithoutRequires;
}
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TestTIdSipAction                                                           *
//******************************************************************************
//* TestTIdSipAction Public methods ********************************************

procedure TestTIdSipAction.SetUp;
begin
  inherited SetUp;

  Self.ActionFailed := false;
end;

//* TestTIdSipAction Protected methods *****************************************

function TestTIdSipAction.CreateAction: TIdSipAction;
begin
  raise Exception.Create(Self.ClassName
                       + ': Don''t call CreateAction on an inbound Action');
end;

procedure TestTIdSipAction.OnAuthenticationChallenge(Action: TIdSipAction;
                                                     Response: TIdSipResponse);
begin
  raise Exception.Create('TestTIdSipAction.OnAuthenticationChallenge');
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
