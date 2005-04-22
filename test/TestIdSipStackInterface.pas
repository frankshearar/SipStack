unit TestIdSipStackInterface;

interface

uses
  IdSipStackInterface, TestFrameworkSip;

type
  TestTIdSipStackInterface = class(TTestCaseTU,
                                   IIdSipStackListener)
  private
    Action:       TIdSipHandle;
    Event:        Cardinal;
    Intf:         TIdSipStackInterface;
    OnEventFired: Boolean;
    Stack:        TIdSipStackInterface;

    function  EstablishCall: TIdSipHandle;
    procedure OnEvent(Stack: TIdSipStackInterface;
                      Action: TIdSipHandle;
                      Event: Cardinal);
    procedure ReceiveByeForOutboundCall;
    procedure ReceiveReInvite;
    procedure ResetOnEventState;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAcceptCall;
    procedure TestAcceptCallWithInvalidHandle;
    procedure TestAcceptCallWithNoExistentHandle;
    procedure TestEndedSession;
    procedure TestEstablishedSessionInboundCall;
    procedure TestEstablishedSessionOutboundCall;
    procedure TestInboundCall;
    procedure TestNetworkFailure;
    procedure TestSessionModifiedByRemoteSide;
  end;

implementation

uses
  IdSipDialog, IdSipMessage, SysUtils, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
  Result.AddTest(TestTIdSipStackInterface.Suite);
end;

//******************************************************************************
//* TestTIdSipStackInterface                                                   *
//******************************************************************************
//* TestTIdSipStackInterface Public methods ************************************

procedure TestTIdSipStackInterface.SetUp;
begin
  inherited SetUp;

  Self.Intf := TIdSipStackInterface.Create(Self.Core);
  Self.Intf.AddListener(Self);

  Self.OnEventFired := false;
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  Self.Intf.Free;

  inherited TearDown;
end;

//* TestTIdSipStackInterface Private methods ***********************************

function TestTIdSipStackInterface.EstablishCall: TIdSipHandle;
begin
  Result := Self.Intf.MakeCall(Self.Destination, '', '');
  Self.Intf.Send(Result);
  Self.ReceiveOk(Self.LastSentRequest);
//  Self.ReceiveAck;

  Self.ResetOnEventState;
end;

procedure TestTIdSipStackInterface.OnEvent(Stack: TIdSipStackInterface;
                                           Action: TIdSipHandle;
                                           Event: Cardinal);
begin
  Self.Action := Action;
  Self.Event  := Event;
  Self.Stack  := Stack;

  Self.OnEventFired := true;
end;

procedure TestTIdSipStackInterface.ReceiveByeForOutboundCall;
var
  LocalDlg: TIdSipDialog;
begin
  LocalDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                Self.Dispatcher.Transport.LastResponse,
                                                false);
  try
    Self.ReceiveBye(LocalDlg);
  finally
    LocalDlg.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveReInvite;
var
  ReInvite: TIdSipRequest;
begin
  ReInvite := TIdSipRequest.Create;
  try
    ReInvite.Assign(Self.LastSentRequest);
    ReInvite.CSeq.Increment;
    ReInvite.LastHop.Branch := ReInvite.LastHop.Branch + '1';

    Self.ReceiveRequest(ReInvite);
  finally
    ReInvite.Free;
  end;
end;

procedure TestTIdSipStackInterface.ResetOnEventState;
begin
  Self.OnEventFired := false;
  Self.Stack        := nil;
  Self.Action       := InvalidHandle;
  Self.Event        := $decafbad;
end;

//* TestTIdSipStackInterface Published methods *********************************

procedure TestTIdSipStackInterface.TestAcceptCall;
const
  Offer = 'offer';
  ContentType = 'content/type';
begin
  Self.ReceiveInvite;

  Self.MarkSentResponseCount;

  Self.Intf.AnswerCall(Self.Action, Offer, ContentType);

  Self.CheckResponseSent('No response sent');
  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Unexpected response sent');
  CheckEquals(Offer,
              Self.LastSentResponse.Body,
              'Offer');
  CheckEquals(ContentType,
              Self.LastSentResponse.ContentType,
              'ContentType');
end;

procedure TestTIdSipStackInterface.TestAcceptCallWithInvalidHandle;
var
  H: TIdSipHandle;
begin
  H := Self.Intf.MakeCall(Self.Destination, '', '');

  try
    Self.Intf.AnswerCall(H, '', '');

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestAcceptCallWithNoExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.AnswerCall(ArbValue, '', '');
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestEndedSession;
var
  Call: TIdSipHandle;
begin
  Call := Self.EstablishCall;

  Self.ResetOnEventState;
  Self.ReceiveByeForOutboundCall;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipSessionEnded, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionInboundCall;
var
  Call: TIdSipHandle;
begin
  Self.ReceiveInvite;
  Check(Self.OnEventFired, 'OnEvent didn''t fire for the inbound call');

  Call := Self.Action;
  Self.ResetOnEventState;
  Self.Intf.AnswerCall(Call, '', '');
  Self.ReceiveAck;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipSessionEstablished, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionOutboundCall;
var
  Call: TIdSipHandle;
begin
  Call := Self.Intf.MakeCall(Self.Destination, '', '');
  Self.Intf.Send(Call);
  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.OnEventFired, 'OnEvent didn''t fire for the inbound call');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipSessionEstablished, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestInboundCall;
begin
  Self.ReceiveInvite;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action > 0, 'Invalid Action handle');
  CheckEquals(SipSessionInbound, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestNetworkFailure;
var
  Call: TIdSipHandle;
begin
  Self.Dispatcher.Transport.FailWith := Exception;
  Call := Self.Intf.MakeCall(Self.Destination, '', '');
  Check(Call > 0, 'Invalid handle');
  Self.Intf.Send(Call);
  Self.DebugTimer.TriggerEarliestEvent;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipNetworkFailure, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestSessionModifiedByRemoteSide;
var
  Call: TIdSipHandle;
begin
  Call := Self.EstablishCall;

  Self.ReceiveReInvite;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipSessionModify, Self.Event, 'Wrong Event param');
end;

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
