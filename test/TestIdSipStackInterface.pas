{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipStackInterface;

interface

uses
  Contnrs, Forms, IdSipMessage, IdSipMockTransport, IdSipStackInterface,
  TestFramework, TestFrameworkEx, TestFrameworkSip;

type
  // The testing of the StackInterface is not completely simple. The UI (or
  // this test case) and the stack-running thread communicate using a Windows
  // message queue. TTestCases don't have a window handle, so we create a
  // TStackWindow which does.
  //
  // As an example, let's look at how TestInboundCall works. The test case
  // sends a SIP message through the (local loopback) network to the stack. The
  // stack sees it's an inbound INVITE, does what it needs to do, and posts the
  // CM_CALL_REQUEST_NOTIFY message to the UI's message queue. The UI picks this
  // up, fires the TestCase's OnEvent. Now we set TestCase.CheckDataProc to
  // point to CheckInboundCallData, so we can check the data.



  // That window does nothing but do stuff to the test
  // cases. That means that you have to keep the message handlers in the
  // TStackWindow in sync with those defined in the StackInterface.

  TDataCheckProc = procedure(Stack: TIdSipStackInterface;
                             Event: Cardinal;
                             Data: TIdEventData) of object;

  TestTIdSipStackInterface = class(TThreadingTestCase)
  private
    CheckDataProc:  TDataCheckProc;
    fIntf:          TIdSipStackInterface;
    DataList:       TObjectList; // Holds all the data received from the stack
    Destination:    TIdSipToHeader;
    LocalMimeType:  String;
    LocalOffer:     String;
    MockTransport:  TIdSipMockUDPTransport;
    Requests:       TIdSipRequestList;
    RemoteMimeType: String;
    RemoteOffer:    String;
//    RemoteUA:       TIdSipUserAgent;
    Responses:      TIdSipResponseList;
//    UdpClient:      TIdUdpClient;
    UI:             TForm;
{

    procedure CheckSessionData(Stack: TIdSipStackInterface;
                               Event: Cardinal;
                               Data: TIdEventData);
    procedure CheckInboundCallData(Stack: TIdSipStackInterface;
                                   Event: Cardinal;
                                   Data: TIdEventData);
}
    procedure CheckNothing(Stack: TIdSipStackInterface;
                           Event: Cardinal;
                           Data: TIdEventData);
{
    procedure CheckRequestSent(const Msg: String);
    procedure CheckResponseSent(const Msg: String);
    function  CreateBindings: TIdSipContacts;
    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateRemoteInvite: TIdSipRequest;
}
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
{
    function  EstablishCall: TIdSipHandle;
    function  LastEventData: TIdEventData;
    function  LastRequest: TIdSipRequest;
    function  LastResponse: TIdSipResponse;
}
    function  LastSentRequest: TIdSipRequest;
{
    function  LastSentResponse: TIdSipResponse;
    procedure LogSentMessage(Msg: TIdSipMessage);
    procedure ReceiveAck;
    procedure ReceiveBusyHereFromRegistrar(Register: TIdSipRequest;
                                           Contacts: TIdSipContacts);

    procedure ReceiveBye(LocalDialog: TIdSipDialog);
    procedure ReceiveByeForOutboundCall;
    procedure ReceiveIntervalTooBrief(Register: TIdSipRequest;
                                      Contacts: TIdSipContacts);
    procedure ReceiveInvite;
    procedure ReceiveInviteWithOffer(const Offer: String;
                                     const MimeType: String);
}
    procedure ReceiveOk(Request: TIdSipRequest);
{
    procedure ReceiveOkWithContacts(Register: TIdSipRequest;
                                    Contacts: TIdSipContacts);
    procedure ReceiveOkWithOffer(Invite: TIdSipRequest;
                                 const Offer: String;
                                 const MimeType: String);
}
    procedure ReceiveRequest(Request: TIdSipRequest);
    procedure ReceiveResponse(Response: TIdSipResponse);
    procedure RecordSentMessage(MsgData: TIdDebugMessageData);
{
    procedure ReceiveReInvite;
}
    function TargetAddress: String;
    function TargetPort: Cardinal;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);

    property Intf: TIdSipStackInterface read fIntf write fIntf;
  published
//    procedure TestAcceptCall;
    procedure TestAcceptCallWithInvalidHandle;
    procedure TestAcceptCallWithNoExistentHandle;
{
    procedure TestEndedSession;
    procedure TestEstablishedSessionInboundCall;
    procedure TestEstablishedSessionOutboundCall;
    procedure TestHangUp;
}
    procedure TestHangUpWithInvalidHandle;
    procedure TestHangUpWithNonExistentHandle;
{
    procedure TestInboundCall;
    procedure TestMakeCall;
    procedure TestMakeRegistration;
    procedure TestModifyCall;
}
    procedure TestModifyCallWithInvalidHandle;
    procedure TestModifyCallWithNonExistentHandle;
    procedure TestOutboundCall;
    procedure TestRedirectCall;
    procedure TestRedirectCallWithInvalidHandle;
    procedure TestRedirectCallWithNonExistentHandle;
    procedure TestRejectCall;
    procedure TestRejectCallWithInvalidHandle;
    procedure TestRejectCallWithNonExistentHandle;
{
//    procedure TestNetworkFailure;
    procedure TestRegistrationFails;
    procedure TestRegistrationFailsWithRetry;
}
    procedure TestSendNonExistentHandle;
//    procedure TestSessionModifiedByRemoteSide;
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

  TestTIdInformationalData = class(TTestCase)
  private
    Data: TIdInformationalData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdAuthenticationChallengeData = class(TTestCase)
  private
    Data: TIdAuthenticationChallengeData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugMessageData = class(TTestCase)
  private
    Data: TIdDebugMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdDebugSendMessageData = class(TTestCase)
  private
    Data: TIdDebugSendMessageData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdFailData = class(TTestCase)
  private
    Data: TIdFailData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdCallEndedData = class(TTestCase)
  private
    Data: TIdCallEndedData;
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

  TestTIdSessionProgressData = class(TTestCase)
  private
    Data: TIdSessionProgressData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdInboundCallData = class(TTestCase)
  private
    Data: TIdInboundCallData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestSetFromStripsTagParam;
  end;

  TestTIdSubscriptionRequestData = class(TTestCase)
  private
    Data: TIdSubscriptionRequestData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
  end;

  TestTIdSubscriptionData = class(TTestCase)
  private
    Data: TIdSubscriptionData;
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
  IdRandom, IdSipLocator, StackWindow, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
//  Result.AddTest(TestTIdSipStackInterface.Suite);
  Result.AddTest(TestTIdEventData.Suite);
  Result.AddTest(TestTIdInformationalData.Suite);
  Result.AddTest(TestTIdAuthenticationChallengeData.Suite);
  Result.AddTest(TestTIdDebugMessageData.Suite);
  Result.AddTest(TestTIdDebugSendMessageData.Suite);
  Result.AddTest(TestTIdFailData.Suite);
  Result.AddTest(TestTIdCallEndedData.Suite);
  Result.AddTest(TestTIdRegistrationData.Suite);
  Result.AddTest(TestTIdFailedRegistrationData.Suite);
  Result.AddTest(TestTIdSessionProgressData.Suite);
  Result.AddTest(TestTIdSessionData.Suite);
  Result.AddTest(TestTIdInboundCallData.Suite);
  Result.AddTest(TestTIdSubscriptionRequestData.Suite);
  Result.AddTest(TestTIdSubscriptionData.Suite);
end;

//******************************************************************************
//* TestTIdSipStackInterface                                                   *
//******************************************************************************
//* TestTIdSipStackInterface Public methods ************************************

procedure TestTIdSipStackInterface.SetUp;
begin
  inherited SetUp;

  Self.CheckDataProc := Self.CheckNothing;
  Self.DataList       := TObjectList.Create(true);
  Self.Destination    := TIdSipToHeader.Create;
  Self.Requests       := TIdSipRequestList.Create;
//  Self.RemoteUA       := TIdSipUserAgent.Create;
  Self.Responses      := TIdSipResponseList.Create;

{
  Self.RemoteUA.Contact.Value := 'sip:case@localhost:5060';
  Self.RemoteUA.From.Value    := 'sip:case@localhost:5060';

  Self.UdpClient      := TIdUdpClient.Create(nil);
  Self.UdpClient.Host := '127.0.0.1';
  Self.UdpClient.Port := 5060;
}
  Self.MockTransport := TIdSipMockUDPTransport.Create;
  Self.UI            := TStackWindow.Create(nil, Self);

  Self.Destination.Value := 'sip:franks@localhost:5060';
  Self.LocalOffer        := Format(DummySdp, ['127.0.0.1']);
  Self.LocalMimeType     := 'application/sdp';
  Self.RemoteOffer       := Format(DummySdp, ['127.0.0.2']);
  Self.RemoteMimeType    := 'application/sdp';
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  Application.ProcessMessages;
  Self.UI.Release;
  Self.MockTransport.Free;
//  Self.UdpClient.Free;
  Self.Responses.Free;
  Self.Requests.Free;
  Self.Destination.Free;
  Self.DataList.Free;

  inherited TearDown;
end;

procedure TestTIdSipStackInterface.OnEvent(Stack: TIdSipStackInterface;
                                           Event: Cardinal;
                                           Data:  TIdEventData);
begin
  case Event of
    CM_DEBUG_SEND_MSG: Self.RecordSentMessage(Data as TIdDebugMessageData);
  else
    Self.CheckDataProc(Stack, Event, Data);
  end;
end;

//* TestTIdSipStackInterface Private methods ***********************************
{
procedure TestTIdSipStackInterface.CheckSessionData(Stack: TIdSipStackInterface;
                                                    Event: Cardinal;
                                                    Data: TIdEventData);
var
  SessionData: TIdSessionData;
begin
  Check(Stack = Self.Intf, 'Wrong Stack param');

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

procedure TestTIdSipStackInterface.CheckInboundCallData(Stack: TIdSipStackInterface;
                                                        Event: Cardinal;
                                                        Data: TIdEventData);
begin
  Check(Stack = Self.Intf, 'Wrong Stack param');
  CheckEquals(CM_CALL_REQUEST_NOTIFY, Event, 'Wrong Event param');
  CheckNotNull(Data, 'Data not present');
  CheckEquals(TIdSessionData.ClassName,
              Data.ClassName,
              'Wrong data');
  Check(Data.Handle > 0, 'Invalid Action handle');
end;
}
procedure TestTIdSipStackInterface.CheckNothing(Stack: TIdSipStackInterface;
                                                Event: Cardinal;
                                                Data: TIdEventData);
begin
  // Use this "Null Object" when you don't actually want to check anything.
end;
{
procedure TestTIdSipStackInterface.CheckRequestSent(const Msg: String);
begin
  raise Exception.Create('implement TestTIdSipStackInterface.CheckRequestSent');
end;

procedure TestTIdSipStackInterface.CheckResponseSent(const Msg: String);
begin
  raise Exception.Create('implement TestTIdSipStackInterface.CheckResponseSent');
end;

function TestTIdSipStackInterface.CreateBindings: TIdSipContacts;
begin
  Result := TIdSipContacts.Create;

  Result.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Result.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

function TestTIdSipStackInterface.CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
begin
  Result := Self.RemoteUA.CreateBye(LocalDialog);
  try
    Result.ToHeader.Tag := LocalDialog.ID.LocalTag;
    Result.From.Tag     := LocalDialog.ID.RemoteTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TestTIdSipStackInterface.CreateRemoteInvite: TIdSipRequest;
begin
  Result := Self.RemoteUA.CreateInvite(Self.Destination, '', '');
end;
}
function TestTIdSipStackInterface.CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
begin
  Result := TIdSipResponse.InResponseTo(Request, SIPOK);
  try
    Result.ToHeader.Tag := GRandomNumber.NextSipUserAgentTag;
  except
    FreeAndNil(Result);

    raise;
  end;
end;
{
function TestTIdSipStackInterface.EstablishCall: TIdSipHandle;
begin
  Result := Self.Intf.MakeCall(Self.Destination,
                               Self.LocalOffer,
                               Self.LocalMimeType);
  Self.Intf.Send(Result);
  CheckRequestSent('No INVITE sent in EstablishCall');

  Self.ReceiveOkWithOffer(Self.LastSentRequest,
                          Self.RemoteOffer,
                          Self.RemoteMimeType);
end;

function TestTIdSipStackInterface.LastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 1] as TIdEventData;
end;

function TestTIdSipStackInterface.LastRequest: TIdSipRequest;
begin
  Result := Self.Requests.Last;
end;

function TestTIdSipStackInterface.LastResponse: TIdSipResponse;
begin
  Result := Self.Responses.Last;
end;
}
function TestTIdSipStackInterface.LastSentRequest: TIdSipRequest;
begin
  Result := Self.Requests.Last;
end;
{
function TestTIdSipStackInterface.LastSentResponse: TIdSipResponse;
begin
  Result := Self.Responses.Last;
end;

procedure TestTIdSipStackInterface.LogSentMessage(Msg: TIdSipMessage);
begin
  if Msg.IsRequest then begin
    Self.Requests.AddCopy(Msg as TIdSipRequest);
  end
  else begin
    Self.Responses.AddCopy(Msg as TIdSipResponse);
  end;
end;

procedure TestTIdSipStackInterface.ReceiveAck;
var
  Ack: TIdSipRequest;
begin
  Ack := Self.LastRequest.AckFor(Self.LastResponse);
  try
    Self.ReceiveRequest(Ack);
  finally
    Ack.Free;
  end;
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

procedure TestTIdSipStackInterface.ReceiveBye(LocalDialog: TIdSipDialog);
var
  Bye: TIdSipRequest;
begin
  Bye := Self.CreateRemoteBye(LocalDialog);
  try
    Self.ReceiveRequest(Bye);
  finally
    Bye.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveByeForOutboundCall;
var
  LocalDlg: TIdSipDialog;
begin
  LocalDlg := TIdSipDialog.CreateOutboundDialog(Self.LastSentRequest,
                                                Self.LastResponse,
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

procedure TestTIdSipStackInterface.ReceiveInvite;
begin
  Self.ReceiveInviteWithOffer('', '');
end;

procedure TestTIdSipStackInterface.ReceiveInviteWithOffer(const Offer: String;
                                                          const MimeType: String);
var
  Invite: TIdSipRequest;
begin
  Invite := Self.CreateRemoteInvite;
  try
    Invite.Body          := Offer;
    Invite.ContentLength := Length(Invite.Body);
    Invite.ContentType   := MimeType;

    Self.ReceiveRequest(Invite);
  finally
    Invite.Free;
  end;
end;
}
procedure TestTIdSipStackInterface.ReceiveOk(Request: TIdSipRequest);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Request);
  try
    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;
{
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
}
procedure TestTIdSipStackInterface.ReceiveRequest(Request: TIdSipRequest);
var
  Dest: TIdSipLocation;
begin
  Self.Requests.AddCopy(Request);

  Dest := TIdSipLocation.Create('UDP', Self.TargetAddress, Self.TargetPort);
  try
    Self.MockTransport.Send(Request, Dest);
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipStackInterface.ReceiveResponse(Response: TIdSipResponse);
var
  Dest: TIdSipLocation;
begin
  Self.Responses.AddCopy(Response);

  Dest := TIdSipLocation.Create('UDP', Self.TargetAddress, Self.TargetPort);
  try
    Self.MockTransport.Send(Response, Dest);
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipStackInterface.RecordSentMessage(MsgData: TIdDebugMessageData);
begin
  if MsgData.Message.IsRequest then
    Self.Requests.AddCopy(MsgData.Message as TIdSipRequest)
  else
    Self.Responses.AddCopy(MsgData.Message as TIdSipResponse)
end;
{
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
    ReInvite.ToHeader.Tag := Self.LastResponse.ToHeader.Tag;

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
}
function TestTIdSipStackInterface.TargetAddress: String;
begin
  Result := (Self.UI as TStackWindow).Address;
end;

function TestTIdSipStackInterface.TargetPort: Cardinal;
begin
  Result := (Self.UI as TStackWindow).Port;
end;

//* TestTIdSipStackInterface Published methods *********************************
{
procedure TestTIdSipStackInterface.TestAcceptCall;
const
  Offer = 'offer';
  ContentType = 'content/type';
begin
end;
}
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
{
procedure TestTIdSipStackInterface.TestEndedSession;
begin
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionInboundCall;
begin
end;

procedure TestTIdSipStackInterface.TestEstablishedSessionOutboundCall;
begin
end;

procedure TestTIdSipStackInterface.TestHangUp;
begin
end;
}
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
{
procedure TestTIdSipStackInterface.TestInboundCall;
begin
  Self.CheckDataProc := Self.CheckInboundCallData;
  Self.ReceiveInvite;
end;

procedure TestTIdSipStackInterface.TestMakeCall;
begin
end;

procedure TestTIdSipStackInterface.TestMakeRegistration;
begin
end;

procedure TestTIdSipStackInterface.TestModifyCall;
begin
  //  ---   INVITE   --->
  // <---   200 OK   ---
  //  ---    ACK     --->
  //  --- (re)INVITE --->
  // <---   200 OK   ---
  //  ---    ACK     --->

  Self.LocalMimeType := 'text/plain';
  Self.LocalOffer    := 'empty';
end;
}
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

procedure TestTIdSipStackInterface.TestOutboundCall;
var
  EventCount: Integer;
  H: TIdSipHandle;
begin
  EventCount := Self.DataList.Count;
  H := Self.Intf.MakeCall(Self.Destination,
                          Self.LocalOffer,
                          Self.LocalMimeType);
  Self.Intf.Send(H);
  Sleep(1000);
  Self.ReceiveOk(Self.LastSentRequest);
  Application.ProcessMessages;
  CheckEquals(EventCount + 1,
              Self.DataList.Count,
              'Event count after established session: where''s the event?');
  CheckEquals(TIdSessionData.ClassName,
              Self.DataList[Self.DataList.Count - 1].ClassName,
              'Unexpected event data');

end;

procedure TestTIdSipStackInterface.TestRedirectCall;
begin
  Fail('Not yet implemented');
end;

procedure TestTIdSipStackInterface.TestRedirectCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "Redirect" a registration attempt.
    Self.Intf.RedirectCall(R, Self.Destination);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRedirectCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.RedirectCall(ArbValue, Self.Destination);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRejectCall;
begin
  Fail('Not yet implemented');
end;

procedure TestTIdSipStackInterface.TestRejectCallWithInvalidHandle;
var
  R: TIdSipHandle;
begin
  R := Self.Intf.MakeRegistration(Self.Destination.Address);

  try
    // You can't, obviously, "reject" a registration attempt.
    Self.Intf.RejectCall(R);

    Fail('No exception raised for an invalid handle');
  except
    on EInvalidHandle do;
  end;
end;

procedure TestTIdSipStackInterface.TestRejectCallWithNonExistentHandle;
const
  ArbValue = 42;
begin
  try
    Self.Intf.RejectCall(ArbValue);
    Fail('No exception raised for a non-existent handle');
  except
    on EInvalidHandle do;
  end;
end;
{
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
  CheckEquals(CM_NETWORK_FAILURE, Self.Event, 'Wrong Event param');

  CheckEquals(TIdFailData.ClassName,
              Self.LastEventData.ClassName,
              'Wrong data');
end;

procedure TestTIdSipStackInterface.TestRegistrationFails;
begin
  //  ---    REGISTER   --->
  // <--- 486 Busy Here ---
end;

procedure TestTIdSipStackInterface.TestRegistrationFailsWithRetry;
begin
  //  ---        REGISTER        --->
  // <--- 423 Interval Too Brief ---
  //  ---        REGISTER        --->
  // <---         200 OK         ---
end;
}
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
{
procedure TestTIdSipStackInterface.TestSessionModifiedByRemoteSide;
begin
  //  ---   INVITE   --->
  // <---   200 OK   ---
  //  ---    ACK     --->
  // <--- (re)INVITE ---
  //  ---   200 OK   --->
  // <---    ACK     ---
end;
}
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
//* TestTIdInformationalData                                                   *
//******************************************************************************
//* TestTIdInformationalData Public methods ************************************

procedure TestTIdInformationalData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdInformationalData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Reason := 'Arbitrary';
end;

procedure TestTIdInformationalData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdInformationalData Published methods ******************************************

procedure TestTIdInformationalData.TestCopy;
var
  Copy: TIdInformationalData;
begin
  Copy := Self.Data.Copy as TIdInformationalData;
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
//* TestTIdAuthenticationChallengeData                                         *
//******************************************************************************
//* TestTIdAuthenticationChallengeData Public methods **************************

procedure TestTIdAuthenticationChallengeData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdAuthenticationChallengeData.Create;
  Self.Data.ChallengedRequest.Method := MethodInvite;
  Self.Data.Challenge.StatusCode     := SIPOK;
  Self.Data.Handle                   := $decafbad;
end;

procedure TestTIdAuthenticationChallengeData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdAuthenticationChallengeData Published methods ***********************

procedure TestTIdAuthenticationChallengeData.TestCopy;
var
  Copy: TIdAuthenticationChallengeData;
begin
  Copy := Self.Data.Copy as TIdAuthenticationChallengeData;
  try
    Check(Self.Data.ChallengedRequest.Equals(Copy.ChallengedRequest),
          'ChallengedRequest');
    Check(Self.Data.Challenge.Equals(Copy.Challenge),
          'Challenge');
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugMessageData                                                    *
//******************************************************************************
//* TestTIdDebugMessageData Public methods *************************************

procedure TestTIdDebugMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugMessageData.Create;
  Self.Data.Message := TIdSipResponse.Create;
end;

procedure TestTIdDebugMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugMessageData Published methods **********************************

procedure TestTIdDebugMessageData.TestCopy;
var
  Copy: TIdDebugMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdDebugSendMessageData                                                *
//******************************************************************************
//* TestTIdDebugSendMessageData Public methods *********************************

procedure TestTIdDebugSendMessageData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdDebugSendMessageData.Create;
  Self.Data.Destination := TIdSipLocation.Create('TCP', '127.0.0.1', 5060);
  Self.Data.Message     := TIdSipRequest.Create;
end;

procedure TestTIdDebugSendMessageData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdDebugSendMessageData Published methods ******************************

procedure TestTIdDebugSendMessageData.TestCopy;
var
  Copy: TIdDebugSendMessageData;
begin
  Copy := Self.Data.Copy as TIdDebugSendMessageData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Destination.AsString,
                Copy.Destination.AsString,
               'The copy''s Destination doesn''t contain the original Destination');
    Check(Copy.Destination <> Self.Data.Destination,
          'The copy contains a reference to the original Destination, not a copy');
    Check(Copy.Message.Equals(Self.Data.Message),
          'The copy''s message doesn''t contain the original message');
    Check(Copy.Message <> Self.Data.Message,
          'The copy contains a reference to the original message, not a copy');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdFailData                                                            *
//******************************************************************************
//* TestTIdFailData Public methods *********************************************

procedure TestTIdFailData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdFailData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Reason := 'Arbitrary';
end;

procedure TestTIdFailData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdFailData Published methods ******************************************

procedure TestTIdFailData.TestCopy;
var
  Copy: TIdFailData;
begin
  Copy := Self.Data.Copy as TIdFailData;
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
//* TestTIdCallEndedData                                                       *
//******************************************************************************
//* TestTIdCallEndedData Public methods ****************************************

procedure TestTIdCallEndedData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdCallEndedData.Create;
  Self.Data.ErrorCode := 42;
  Self.Data.Handle    := $decafbad;
  Self.Data.Reason    := 'Arbitrary';
end;

procedure TestTIdCallEndedData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdCallEndedData Published methods ******************************************

procedure TestTIdCallEndedData.TestCopy;
var
  Copy: TIdCallEndedData;
begin
  Copy := Self.Data.Copy as TIdCallEndedData;
  try
    CheckEquals(Self.Data.ErrorCode,
                Copy.ErrorCode,
                'ErrorCode');
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
    CheckEquals(Self.Data.ErrorCode,
                Copy.ErrorCode,
                'ErrorCode');
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

//******************************************************************************
//* TestTIdSessionProgressData                                                 *
//******************************************************************************
//* TestTIdSessionProgressData Public methods **********************************

procedure TestTIdSessionProgressData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSessionProgressData.Create;

  Self.Data.Banner                   := 'Fake TextDirect banner';
  Self.Data.Handle                   := $decafbad;
  Self.Data.LocalMimeType            := 'text/html';
  Self.Data.LocalSessionDescription  := '<html />';
  Self.Data.ProgressCode             := SIPSessionProgress;
  Self.Data.RemoteMimeType           := 'text/plain';
  Self.Data.RemoteSessionDescription := 'random data';
end;

procedure TestTIdSessionProgressData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSessionProgressData Published methods *******************************

procedure TestTIdSessionProgressData.TestCopy;
var
  Copy: TIdSessionProgressData;
begin
  Copy := Self.Data.Copy as TIdSessionProgressData;
  try
    CheckEquals(Self.Data.Banner,
                Copy.Banner,
                'Banner');
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.LocalSessionDescription,
                Copy.LocalSessionDescription,
                'LocalSessionDescription');
    CheckEquals(Self.Data.ProgressCode,
                Copy.ProgressCode,
                'ProgressCode');
    CheckEquals(Self.Data.RemoteMimeType,
                Copy.RemoteMimeType,
                'RemoteMimeType');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdInboundCallData                                                     *
//******************************************************************************
//* TestTIdInboundCallData Public methods **************************************

procedure TestTIdInboundCallData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdInboundCallData.Create;
  Self.Data.From.Value    := 'J. Random Loser <sip:loser@always.a.loser.com>';
  Self.Data.Contact.Value := 'J. Random Loser <sip:loser@[::1]>';
  Self.Data.Handle                   := $decafbad;
  Self.Data.LocalMimeType            := '1';
  Self.Data.LocalSessionDescription  := '2';
  Self.Data.RemoteMimeType           := '3';
  Self.Data.RemoteSessionDescription := '4';
end;

procedure TestTIdInboundCallData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdInboundCallData Published methods ***********************************

procedure TestTIdInboundCallData.TestCopy;
var
  Copy: TIdInboundCallData;
begin
  Copy := Self.Data.Copy as TIdInboundCallData;
  try
    CheckEquals(Self.Data.Contact.Value,
                Copy.Contact.Value,
                'Contact');
    CheckEquals(Self.Data.From.Value,
                Copy.From.Value,
                'From');
    CheckEquals(Self.Data.LocalMimeType,
                Copy.LocalMimeType,
                'LocalMimeType');
    CheckEquals(Self.Data.LocalSessionDescription,
                Copy.LocalSessionDescription,
                'LocalSessionDescription');
    CheckEquals(Self.Data.RemoteMimeType,
                Copy.RemoteMimeType,
                'RemoteMimeType');
    CheckEquals(Self.Data.RemoteSessionDescription,
                Copy.RemoteSessionDescription,
                'RemoteSessionDescription');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdInboundCallData.TestSetFromStripsTagParam;
var
  Copy: TIdInboundCallData;
begin
  Self.Data.From.Tag := 'foofoo';

  Copy := TIdInboundCallData.Create;
  try
    Copy.From := Self.Data.From;
    Check(not Copy.From.HasTag, 'Tag param not removed');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSubscriptionData                                                    *
//******************************************************************************
//* TestTIdSubscriptionData Public methods *************************************

procedure TestTIdSubscriptionData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSubscriptionData.Create;
  Self.Data.Handle := $decafbad;
  Self.Data.Notify.RequestUri.Uri := 'sip:case@fried-neurons.org';
end;

procedure TestTIdSubscriptionData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSubscriptionData Published methods **********************************

procedure TestTIdSubscriptionData.TestCopy;
var
  Copy: TIdSubscriptionData;
begin
  Copy := Self.Data.Copy as TIdSubscriptionData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    Check(Copy.Notify.Equals(Self.Data.Notify),
          'NOTIFY messages don''t match');
  finally
    Copy.Free;
  end;
end;

//******************************************************************************
//* TestTIdSubscriptionRequestData                                             *
//******************************************************************************
//* TestTIdSubscriptionRequestData Public methods ******************************

procedure TestTIdSubscriptionRequestData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdSubscriptionRequestData.Create;
  Self.Data.Contact.Value := 'sip:machine-1@internet-cafe.org>';
  Self.Data.EventPackage  := PackageRefer;
  Self.Data.From.Value    := 'Case <sip:case@fried-neurons.org>';
  Self.Data.ReferTo.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna';
end;

procedure TestTIdSubscriptionRequestData.TearDown;
begin
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSubscriptionRequestData Published methods ***************************

procedure TestTIdSubscriptionRequestData.TestCopy;
var
  Copy: TIdSubscriptionRequestData;
begin
  Copy := Self.Data.Copy as TIdSubscriptionRequestData;
  try
    CheckEquals(IntToHex(Self.Data.Handle, 8),
                IntToHex(Copy.Handle, 8),
                'Handle');
    CheckEquals(Self.Data.Contact.FullValue,
                Copy.Contact.FullValue,
                'Contact');
    CheckEquals(Self.Data.EventPackage,
                Copy.EventPackage,
                'EventPackage');
    CheckEquals(Self.Data.From.FullValue,
                Copy.From.FullValue,
                'From');
    CheckEquals(Self.Data.ReferTo.FullValue,
                Copy.ReferTo.FullValue,
                'ReferTo');
  finally
    Copy.Free;
  end;
end;

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
