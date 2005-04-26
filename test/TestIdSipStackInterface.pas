unit TestIdSipStackInterface;

interface

uses
  Contnrs, IdSipMessage, IdSipStackInterface, TestFramework, TestFrameworkSip;

type
  TestTIdSipStackInterface = class(TTestCaseTU,
                                   IIdSipStackListener)
  private
    Action:         TIdSipHandle;
    DataList:       TObjectList;
    Event:          Cardinal;
    Intf:           TIdSipStackInterface;
    LocalMimeType:  String;
    LocalOffer:     String;
    OnEventFired:   Boolean;
    RemoteMimeType: String;
    RemoteOffer:    String;
    Stack:          TIdSipStackInterface;

    procedure CheckData(Data: TIdEventData);
    function  CreateBindings: TIdSipContacts;
    function  EstablishCall: TIdSipHandle;
    function  LastEventData: TIdEventData;
    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);
    procedure ReceiveBusyHereFromRegistrar(Register: TIdSipRequest;
                                           Contacts: TIdSipContacts);
    procedure ReceiveByeForOutboundCall;
    procedure ReceiveIntervalTooBrief(Register: TIdSipRequest;
                                      Contacts: TIdSipContacts);
    procedure ReceiveInviteWithOffer(const Offer: String;
                                     const MimeType: String);
    procedure ReceiveOkWithContacts(Register: TIdSipRequest;
                                    Contacts: TIdSipContacts);
    procedure ReceiveOkWithOffer(Invite: TIdSipRequest;
                                 const Offer: String;
                                 const MimeType: String);
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
    procedure TestHangUp;
    procedure TestHangUpWithInvalidHandle;
    procedure TestHangUpWithNonExistentHandle;
    procedure TestInboundCall;
    procedure TestMakeCall;
    procedure TestMakeRegistration;
    procedure TestModifyCall;
    procedure TestModifyCallWithInvalidHandle;
    procedure TestModifyCallWithNonExistentHandle;
    procedure TestNetworkFailure;
    procedure TestRegistrationFails;
    procedure TestRegistrationFailsWithRetry;
    procedure TestSendNonExistentHandle;
    procedure TestSessionModifiedByRemoteSide;
  end;

  TestTIdEventData = class(TTestCase)
  private
    Data: TIdEventData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdFailEventData = class(TTestCase)
  private
    Data: TIdFailEventData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdRegistrationData = class(TTestCase)
  private
    Data: TIdRegistrationData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestSetContacts;
  end;

  TestTIdFailedRegistrationData = class(TTestCase)
  private
    Data: TIdFailedRegistrationData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSessionData = class(TTestCase)
  private
    Data: TIdSessionData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

const
  DummySdp = 'v=0'#13#10
           + 'o=sc 1105373135 1105373135 IN IP4 %s'#13#10
           + 's=Dummy on hold SDP'#13#10
           + 'c=IN IP4 0.0.0.0'#13#10
           + 'm=audio 65534 RTP/AVP 0'#13#10
           + 'a=rtpmap:0 PCMU/8000'#13#10
           + 'a=recvonly'#13#10;


implementation

uses
  IdSipDialog, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
  Result.AddTest(TestTIdSipStackInterface.Suite);
  Result.AddTest(TestTIdEventData.Suite);
  Result.AddTest(TestTIdFailEventData.Suite);
  Result.AddTest(TestTIdRegistrationData.Suite);
  Result.AddTest(TestTIdFailedRegistrationData.Suite);
  Result.AddTest(TestTIdSessionData.Suite);
end;

//******************************************************************************
//* TestTIdSipStackInterface                                                   *
//******************************************************************************
//* TestTIdSipStackInterface Public methods ************************************

procedure TestTIdSipStackInterface.SetUp;
begin
  inherited SetUp;

  Self.DataList := TObjectList.Create(true);

  Self.Intf := TIdSipStackInterface.Create(Self.Core);
  Self.Intf.AddListener(Self);

  Self.OnEventFired := false;

  Self.LocalOffer     := Format(DummySdp, ['127.0.0.1']);
  Self.LocalMimeType  := 'application/sdp';
  Self.RemoteOffer    := Format(DummySdp, ['127.0.0.2']);
  Self.RemoteMimeType := 'application/sdp';
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  Self.Intf.Free;
  Self.DataList.Free;

  inherited TearDown;
end;

//* TestTIdSipStackInterface Private methods ***********************************

procedure TestTIdSipStackInterface.CheckData(Data: TIdEventData);
var
  SessionData: TIdSessionData;
begin
  CheckNotNull(Data, 'Data not present');
  CheckEquals(TIdSessionData.ClassName,
              Data.ClassName,
              'Type of data');

  SessionData := Data as TIdSessionData;

  CheckEquals(Self.LocalOffer,
              SessionData.LocalSessionDescription,
              'LocalSessionDescription');
  CheckEquals(Self.LocalMimeType,
              SessionData.LocalMimeType,
              'LocalMimeType');
  CheckEquals(Self.RemoteOffer,
              SessionData.RemoteSessionDescription,
              'RemoteSessionDescription');
  CheckEquals(Self.RemoteMimeType,
              SessionData.RemoteMimeType,
              'RemoteMimeType');
end;

function TestTIdSipStackInterface.CreateBindings: TIdSipContacts;
begin
  Result := TIdSipContacts.Create;

  Result.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Result.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

function TestTIdSipStackInterface.EstablishCall: TIdSipHandle;
begin
  Result := Self.Intf.MakeCall(Self.Destination,
                               Self.LocalOffer,
                               Self.LocalMimeType);
  Self.Intf.Send(Result);
  Self.ReceiveOkWithOffer(Self.LastSentRequest,
                          Self.RemoteOffer,
                          Self.RemoteMimeType);

  Self.ResetOnEventState;
end;

function TestTIdSipStackInterface.LastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 1] as TIdEventData;
end;

procedure TestTIdSipStackInterface.OnEvent(Stack: TIdSipStackInterface;
                                           Event: Cardinal;
                                           Data:  TIdEventData);
begin
  Self.Action := Data.Handle;
  Self.DataList.Add(Data.Copy);

  Self.Event  := Event;
  Self.Stack  := Stack;

  Self.OnEventFired := true;
end;

procedure TestTIdSipStackInterface.ReceiveBusyHereFromRegistrar(Register: TIdSipRequest;
                                                                Contacts: TIdSipContacts);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.StatusCode := SIPBusyHere;
    Response.Contacts.Clear;
    Response.Contacts.Add(Contacts);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
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

procedure TestTIdSipStackInterface.ReceiveIntervalTooBrief(Register: TIdSipRequest;
                                                           Contacts: TIdSipContacts);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.StatusCode := SIPIntervalTooBrief;
    Response.AddHeader(MinExpiresHeader).Value := '1000';
    Response.Contacts.Clear;
    Response.Contacts.Add(Contacts);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveInviteWithOffer(const Offer: String;
                                                          const MimeType: String);
begin
  Self.Invite.Body          := Offer;
  Self.Invite.ContentLength := Length(Self.Invite.Body);
  Self.Invite.ContentType   := MimeType;

  Self.ReceiveRequest(Self.Invite);
end;

procedure TestTIdSipStackInterface.ReceiveOkWithContacts(Register: TIdSipRequest;
                                                         Contacts: TIdSipContacts);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Register);
  try
    Response.Contacts.Clear;
    Response.Contacts.Add(Contacts);
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveOkWithOffer(Invite: TIdSipRequest;
                                                      const Offer: String;
                                                      const MimeType: String);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Invite);
  try
    Response.Body          := Offer;
    Response.ContentLength := Length(Response.Body);
    Response.ContentType   := MimeType;
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveReInvite;
var
  ReInvite: TIdSipRequest;
  Temp:     String;
begin
  // Precondition: A full call has been established, so LastSentRequest and
  // LastResponse both point to meaningful messages.

  ReInvite := TIdSipRequest.Create;
  try
    ReInvite.Assign(Self.LastSentRequest);

    ReInvite.CSeq.Increment;
    ReInvite.LastHop.Branch := ReInvite.LastHop.Branch + '1';
    ReInvite.ToHeader.Tag := Self.Dispatcher.Transport.LastResponse.ToHeader.Tag;

    // This message comes FROM the network so its From/To tags are the reverse
    // of the outbound INVITE's
    Temp                  := ReInvite.From.Tag;
    ReInvite.From.Tag     := ReInvite.ToHeader.Tag;
    ReInvite.ToHeader.Tag := Temp;

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
    // Of course, you can't answer an outbound call.
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
  Self.ReceiveInviteWithOffer(Self.RemoteOffer, Self.RemoteMimeType);
  Check(Self.OnEventFired, 'OnEvent didn''t fire for the inbound call');

  Call := Self.Action;
  Self.ResetOnEventState;
  Self.Intf.AnswerCall(Call, Self.LocalOffer, Self.LocalMimeType);
  Self.ReceiveAck;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckData(Self.LastEventData);
  CheckEquals(SipSessionEstablished, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionOutboundCall;
var
  Call: TIdSipHandle;
begin
  Call := Self.Intf.MakeCall(Self.Destination, Self.LocalOffer, Self.LocalMimeType);
  Self.Intf.Send(Call);
  Self.ReceiveOkWithOffer(Self.LastSentRequest, Self.RemoteOffer, Self.RemoteMimeType);

  Check(Self.OnEventFired, 'OnEvent didn''t fire for the inbound call');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckData(Self.LastEventData);
  CheckEquals(SipSessionEstablished, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestHangUp;
var
  Call: TIdSipHandle;
begin
  Call := Self.EstablishCall;

  Self.MarkSentRequestCount;

  Self.Intf.HangUp(Call);

  Self.CheckRequestSent('No BYE sent');
  CheckEquals(MethodBye,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
end;

procedure TestTIdSipStackInterface.TestHangUpWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "hang up" a registration attempt.
    Self.Intf.HangUp(R);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestHangUpWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.HangUp(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestInboundCall;
begin
  Self.ReceiveInvite;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  Check(Self.Action > 0, 'Invalid Action handle');
  CheckEquals(SipSessionInbound, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestMakeCall;
var
  Call: TIdSipHandle;
begin
  Call := Self.Intf.MakeCall(Self.Destination,
                               Self.LocalOffer,
                               Self.LocalMimeType);
  Check(Call > 0, 'MakeCall returned an invalid handle');

  Self.MarkSentRequestCount;
  Self.Intf.Send(Call);
  CheckRequestSent('No INVITE sent');
  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              'Unexpected request sent');

  Self.ReceiveOk(Self.LastSentRequest);

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipSessionEstablished, Self.Event, 'Wrong Event param');
end;

procedure TestTIdSipStackInterface.TestMakeRegistration;
var
  Bindings: TIdSipContacts;
  R:        TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);
  Check(R > 0, 'MakeRegistration returned an invalid handle');

  Self.MarkSentRequestCount;
  Self.Intf.Send(R);
  CheckRequestSent('No REGISTER sent');
  CheckEquals(MethodRegister,
              Self.LastSentRequest.Method,
              'Unexpected request sent');

  Bindings := Self.CreateBindings;
  try
    Self.ReceiveOkWithContacts(Self.LastSentRequest, Bindings);

    Check(Self.OnEventFired, 'OnEvent didn''t fire');
    Check(Self.Action = R, 'Invalid Action handle');
    CheckEquals(SipSuccess, Self.Event, 'Wrong Event param');

    CheckEquals(TIdRegistrationData.ClassName,
                Self.LastEventData.ClassName,
                'Wrong data');

    Check(Bindings.Equals((Self.LastEventData as TIdRegistrationData).Contacts),
          'Wrong bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestModifyCall;
var
  Call: TIdSipHandle;
begin
  Self.LocalMimeType := 'text/plain';
  Self.LocalOffer    := 'empty';

  Call := Self.EstablishCall;

  Self.MarkSentRequestCount;
  Self.Intf.ModifyCall(Call, Self.LocalOffer, Self.LocalMimeType);
  CheckRequestSent('No re-INVITE sent');
  CheckEquals(MethodInvite,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
  CheckEquals(Self.LocalOffer,
              Self.LastSentRequest.Body,
              'Offer');
  CheckEquals(Self.LocalMimeType,
              Self.LastSentRequest.ContentType,
              'Content-Type');
end;

procedure TestTIdSipStackInterface.TestModifyCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "modify" a registration attempt.
    Self.Intf.ModifyCall(R, Self.LocalOffer, Self.LocalMimeType);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestModifyCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.ModifyCall(ArbValue, '', '');
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
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
  Check(Self.Action = Call, 'Invalid Action handle');
  CheckEquals(SipNetworkFailure, Self.Event, 'Wrong Event param');

  CheckEquals(TIdFailEventData.ClassName,
              Self.LastEventData.ClassName,
              'Wrong data');
end;

procedure TestTIdSipStackInterface.TestRegistrationFails;
var
  Bindings:     TIdSipContacts;
  R:            TIdSipHandle;
  ReturnedData: TIdFailedRegistrationData;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);
  Self.Intf.Send(R);

  Bindings := Self.CreateBindings;
  try
    Self.ReceiveBusyHereFromRegistrar(Self.LastSentRequest, Bindings);

    Check(Self.OnEventFired, 'OnEvent didn''t fire');
    Check(Self.Action = R, 'Invalid Action handle');
    CheckEquals(SipFailure, Self.Event, 'Wrong Event param');

    CheckEquals(TIdFailedRegistrationData.ClassName,
                Self.LastEventData.ClassName,
                'Wrong data');

    ReturnedData := Self.LastEventData as TIdFailedRegistrationData;
    Check(Bindings.Equals(ReturnedData.Contacts),
          'Wrong bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestRegistrationFailsWithRetry;
var
  Bindings:     TIdSipContacts;
  R:            TIdSipHandle;
  ReturnedData: TIdRegistrationData;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);
  Self.Intf.Send(R);

  Bindings := Self.CreateBindings;
  try
    Self.ReceiveIntervalTooBrief(Self.LastSentRequest, Bindings);
    Self.ReceiveOkWithContacts(Self.LastSentRequest, Bindings);

    Check(Self.OnEventFired, 'OnEvent didn''t fire');
    Check(Self.Action = R, 'Invalid Action handle');
    CheckEquals(SipSuccess, Self.Event, 'Wrong Event param');

    CheckEquals(TIdRegistrationData.ClassName,
                Self.LastEventData.ClassName,
                'Wrong data');

    ReturnedData := Self.LastEventData as TIdRegistrationData;
    Check(Bindings.Equals(ReturnedData.Contacts),
          'Wrong bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipStackInterface.TestSendNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.Send(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestSessionModifiedByRemoteSide;
var
  Call: TIdSipHandle;
begin
  Call := Self.EstablishCall;

  Self.ReceiveReInvite;

  Check(Self.OnEventFired, 'OnEvent didn''t fire');
  Check(Self.Stack = Self.Intf, 'Wrong Stack param');
  CheckEquals(SipSessionModify, Self.Event, 'Wrong Event param');
  Check(Self.Action = Call, 'Invalid Action handle');
end;

//******************************************************************************
//* TestTIdEventData                                                           *
//******************************************************************************
//* TestTIdEventData Public methods ********************************************

procedure TestTIdEventData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdEventData.Create;
  Self.Data.Handle := $decafbad;
end;

procedure TestTIdEventData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdEventData Published methods *****************************************

procedure TestTIdEventData.TestCopy;
var
  Copy: TIdEventData;
begin
  Copy := Self.Data.Copy;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailEventData                                                       *
//******************************************************************************
//* TestTIdFailEventData Public methods ****************************************

procedure TestTIdFailEventData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdFailEventData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Reason := 'Arbitrary';
end;

procedure TestTIdFailEventData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdFailEventData Published methods *************************************

procedure TestTIdFailEventData.TestCopy;
var
  Copy: TIdFailEventData;
begin
  Copy := Self.Data.Copy as TIdFailEventData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdRegistrationData                                                    *
//******************************************************************************
//* TestTIdRegistrationData Public methods *************************************

procedure TestTIdRegistrationData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdRegistrationData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

procedure TestTIdRegistrationData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdRegistrationData Published methods **********************************

procedure TestTIdRegistrationData.TestCopy;
var
  Copy: TIdRegistrationData;
begin
  Copy := Self.Data.Copy as TIdRegistrationData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Self.Data.Contacts.Equals(Copy.Contacts),
          'Contacts');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdRegistrationData.TestSetContacts;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(ContactHeaderFull).Value := 'sip:cthulhu@rlyeh.org';
    Bindings.Add(ContactHeaderFull).Value := 'sip:azathoth@centre-of-all-infinity.org';

    Self.Data.Contacts := Bindings;

    Check(Bindings.Equals(Self.Data.Contacts),
          'Setter didn''t set Contacts');
  finally
    Bindings.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailedRegistrationData                                              *
//******************************************************************************
//* TestTIdFailedRegistrationData Public methods *******************************

procedure TestTIdFailedRegistrationData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdFailedRegistrationData.Create;
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Self.Data.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Data.Reason := 'For no good reason';
end;

procedure TestTIdFailedRegistrationData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdFailedRegistrationData Private methods ******************************

procedure TestTIdFailedRegistrationData.TestCopy;
var
  Copy: TIdFailedRegistrationData;
begin
  Copy := Self.Data.Copy as TIdFailedRegistrationData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Self.Data.Contacts.Equals(Copy.Contacts),
          'Contacts');
    CheckEquals(Self.Data.Reason,
                Copy.Reason,
                'Reason');      
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSessionData                                                         *
//******************************************************************************
//* TestTIdSessionData Public methods ******************************************

procedure TestTIdSessionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionData.Create;
  Self.Data.LocalSessionDescription  := '1';
  Self.Data.LocalMimeType            := '2';
  Self.Data.RemoteSessionDescription := '3';
  Self.Data.RemoteMimeType           := '4';
end;

procedure TestTIdSessionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSessionData Published methods ***************************************

procedure TestTIdSessionData.TestCopy;
var
  Copy: TIdSessionData;
begin
  Copy := Self.Data.Copy as TIdSessionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.LocalSessionDescription,
                Copy.LocalSessionDescription,
                'LocalSessionDescription');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
    CheckEquals(Self.Data.RemoteMimeType,
                Copy.RemoteMimeType,
                'RemoteMimeType');
  finally
    Copy.Free;
  end;
end;

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
