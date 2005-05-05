unit TestIdSipStackInterface;

interface

uses
  Contnrs, Forms, IdSipCore, IdSipDialog, IdSipMessage, IdSipStackInterface,
  IdSipTransport, IdTimerQueue, IdUDPClient, SyncObjs, SysUtils, TestFramework,
  TestFrameworkSip;

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

  TestTIdSipStackInterface = class(TTestCase)
  private
    CheckDataProc:  TDataCheckProc;
    fIntf:          TIdSipStackInterface;

    DataList:       TObjectList;
    Destination:    TIdSipToHeader;
    LocalMimeType:  String;
    LocalOffer:     String;
    Requests:       TIdSipRequestList;
    RemoteMimeType: String;
    RemoteOffer:    String;
    RemoteUA:       TIdSipUserAgent;
    Responses:      TIdSipResponseList;
    UdpClient:      TIdUdpClient;
    UI:             TForm;

    procedure CheckSessionData(Stack: TIdSipStackInterface;
                               Event: Cardinal;
                               Data: TIdEventData);
    procedure CheckInboundCallData(Stack: TIdSipStackInterface;
                                   Event: Cardinal;
                                   Data: TIdEventData);
    procedure CheckNothing(Stack: TIdSipStackInterface;
                           Event: Cardinal;
                           Data: TIdEventData);
    procedure CheckRequestSent(const Msg: String);
    procedure CheckResponseSent(const Msg: String);
    function  CreateBindings: TIdSipContacts;
    function  CreateRemoteBye(LocalDialog: TIdSipDialog): TIdSipRequest;
    function  CreateRemoteInvite: TIdSipRequest;
    function  CreateRemoteOk(Request: TIdSipRequest): TIdSipResponse;
    function  EstablishCall: TIdSipHandle;
    function  LastEventData: TIdEventData;
    function  LastRequest: TIdSipRequest;
    function  LastResponse: TIdSipResponse;
    function  LastSentResponse: TIdSipResponse;
    function  LastSentRequest: TIdSipRequest;
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
    procedure ReceiveOk(Request: TIdSipRequest);
    procedure ReceiveOkWithContacts(Register: TIdSipRequest;
                                    Contacts: TIdSipContacts);
    procedure ReceiveOkWithOffer(Invite: TIdSipRequest;
                                 const Offer: String;
                                 const MimeType: String);
    procedure ReceiveRequest(Request: TIdSipRequest);
    procedure ReceiveResponse(Response: TIdSipResponse);
    procedure ReceiveReInvite;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);
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
//    procedure TestNetworkFailure;
    procedure TestRegistrationFails;
    procedure TestRegistrationFailsWithRetry;
    procedure TestSendNonExistentHandle;
    procedure TestSessionModifiedByRemoteSide;

    property Intf: TIdSipStackInterface read fIntf write fIntf;
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
  IdRandom, StackWindow;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipStackInterface unit tests');
  Result.AddTest(TestTIdSipStackInterface.Suite);
  Result.AddTest(TestTIdEventData.Suite);
  Result.AddTest(TestTIdFailEventData.Suite);
  Result.AddTest(TestTIdRegistrationData.Suite);
  Result.AddTest(TestTIdFailedRegistrationData.Suite);
  Result.AddTest(TestTIdSessionData.Suite);
  Result.AddTest(TestTIdInboundCallData.Suite);
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
  Self.RemoteUA       := TIdSipUserAgent.Create;
  Self.Responses      := TIdSipResponseList.Create;
  Self.UI             := TStackWindow.Create(nil, Self);

  Self.Destination.Value      := 'sip:franks@localhost:5060';
  Self.RemoteUA.Contact.Value := 'sip:case@localhost:5060';
  Self.RemoteUA.From.Value    := 'sip:case@localhost:5060';

  Self.UdpClient      := TIdUdpClient.Create(nil);
  Self.UdpClient.Host := '127.0.0.1';
  Self.UdpClient.Port := 5060;

  Self.LocalOffer     := Format(DummySdp, ['127.0.0.1']);
  Self.LocalMimeType  := 'application/sdp';
  Self.RemoteOffer    := Format(DummySdp, ['127.0.0.2']);
  Self.RemoteMimeType := 'application/sdp';
end;

procedure TestTIdSipStackInterface.TearDown;
begin
  Application.ProcessMessages;
  Self.UI.Release;

  Self.UdpClient.Free;
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
  Self.CheckDataProc(Stack, Event, Data);
end;

//* TestTIdSipStackInterface Private methods ***********************************

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

procedure TestTIdSipStackInterface.CheckNothing(Stack: TIdSipStackInterface;
                                                Event: Cardinal;
                                                Data: TIdEventData);
begin
  // Use this "Null Object" when you don't actually want to check anything.
end;

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

function TestTIdSipStackInterface.LastSentResponse: TIdSipResponse;
begin
  Result := Self.Responses.Last;
end;

function TestTIdSipStackInterface.LastSentRequest: TIdSipRequest;
begin
  Result := Self.Requests.Last;
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

procedure TestTIdSipStackInterface.ReceiveRequest(Request: TIdSipRequest);
begin
  Self.Requests.AddCopy(Request);
  Self.UdpClient.Send(Request.AsString);
end;

procedure TestTIdSipStackInterface.ReceiveResponse(Response: TIdSipResponse);
begin
  Self.Responses.AddCopy(Response);
  Self.UdpClient.Send(Response.AsString);
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

//* TestTIdSipStackInterface Published methods *********************************

procedure TestTIdSipStackInterface.TestAcceptCall;
const
  Offer = 'offer';
  ContentType = 'content/type';
begin
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

  CheckEquals(TIdFailEventData.ClassName,
              Self.LastEventData.ClassName,
              'Wrong data');
end;
}
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
begin
  //  ---   INVITE   --->
  // <---   200 OK   ---
  //  ---    ACK     --->
  // <--- (re)INVITE ---
  //  ---   200 OK   --->
  // <---    ACK     ---
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

//******************************************************************************
//* TestTIdInboundCallData                                                     *
//******************************************************************************
//* TestTIdInboundCallData Public methods **************************************

procedure TestTIdInboundCallData.SetUp;
begin
  inherited SetUp;

  Self.Data := TIdInboundCallData.Create;
  Self.Data.From.Value := 'J. Random Loser <sip:loser@always.a.loser.com>';
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

initialization
  RegisterTest('SIP stack interface tests', Suite);
end.
