{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkSip;

interface

uses
  Classes, IdInterfacedObject, IdObservable, IdRTP, IdSdp, IdSipMessage,
  IdSipCore, IdSipDialog, IdSipTcpClient, IdSipTcpServer, IdSipTransaction,
  IdSipTransport, SysUtils, TestFrameworkEx;

type
  TIdSipTestResources = class(TObject)
  private
    class function CreateCommonRequest: TIdSipRequest;
  public
    class function CreateBasicRequest: TIdSipRequest;
    class function CreateBasicResponse: TIdSipResponse;
    class function CreateLocalLoopRequest: TIdSipRequest;
    class function CreateLocalLoopResponse: TIdSipResponse;
    class function BasicSDP(const Host: String): String;
    class function VeryLargeSDP(const Host: String): String;
  end;

  TTestCaseSip = class(TThreadingTestCase)
  public
    procedure CheckEquals(Expected,
                          Received: TIdSipURI;
                          const Msg: String); overload;
    procedure CheckEquals(Expected,
                          Received: TIdSipHeadersFilter;
                          const Msg: String); overload;
  end;

  TIdSipExceptionRaisingHeader = class(TIdSipHeader)
  protected
    procedure Parse(const Value: String); override;
  end;

  TIdSipMockListener = class(TIdInterfacedObject,
                             IIdSipActionListener)
  private
    fFailWith:      ExceptClass;
    fNetworkFailed: Boolean;

    procedure OnNetworkFailure(Action: TIdSipAction;
                               const Reason: String);
  protected
    fReasonParam: String;
  public
    constructor Create; virtual;

    property FailWith:      ExceptClass read fFailWith write fFailWith;
    property NetworkFailed: Boolean     read fNetworkFailed;
    property ReasonParam:   String      read fReasonParam;
  end;

  TIdSipTestDataListener = class(TIdSipMockListener,
                                 IIdRtpDataListener)
  private
    fNewData:    Boolean;
    fNewUdpData: Boolean;
  public
    constructor Create; override;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnNewUdpData(Data: TStream);

    property NewData:    Boolean read fNewData;
    property NewUdpData: Boolean read fNewUdpData;
  end;

  TIdSipTestMessageListener = class(TIdSipMockListener,
                                    IIdSipMessageListener)
  private
    fException:             Boolean;
    fExceptionParam:        Exception;
    fMalformedMessage:      Boolean;
    fMalformedMessageParam: String;
    fReceivedFromParam:     TIdSipConnectionBindings;
    fReceivedRequest:       Boolean;
    fReceivedResponse:      Boolean;
    fRequestParam:          TIdSipRequest;
    fResponseParam:         TIdSipResponse;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  public
    constructor Create; override;

    property Exception:             Boolean                  read fException;
    property ExceptionParam:        Exception                read fExceptionParam;
    property MalformedMessage:      Boolean                  read fMalformedMessage;
    property MalformedMessageParam: String                   read fMalformedMessageParam;
    property ReceivedFromParam:     TIdSipConnectionBindings read fReceivedFromParam;
    property ReceivedRequest:       Boolean                  read fReceivedRequest;
    property ReceivedResponse:      Boolean                  read fReceivedResponse;
    property RequestParam:          TIdSipRequest            read fRequestParam;
    property ResponseParam:         TIdSipResponse           read fResponseParam;
  end;

  TIdSipTestObserver = class(TIdSipMockListener,
                             IIdObserver)
  private
    fChanged: Boolean;

    procedure OnChanged(Observed: TObject);
  public
    constructor Create; override;

    property Changed: Boolean read fChanged;
  end;

  TIdSipTestInboundInviteListener = class(TIdSipMockListener,
                                          IIdSipInboundInviteListener)
  private
    fAckParam:          TIdSipRequest;
    fFailed:            Boolean;
    fInviteAgentParam:  TIdSipInboundInvite;
    fSucceeded:         Boolean;

    procedure OnFailure(InviteAgent: TIdSipInboundInvite);
    procedure OnSuccess(InviteAgent: TIdSipInboundInvite;
                        Ack: TIdSipRequest);
  public
    constructor Create; override;

    property AckParam:         TIdSipRequest       read fAckParam;
    property Failed:           Boolean             read fFailed;
    property InviteAgentParam: TIdSipInboundInvite read fInviteAgentParam;
    property Succeeded:        Boolean             read fSucceeded;
  end;


  TIdSipTestInviteListener = class(TIdSipMockListener,
                                   IIdSipInviteListener)
  private
    fDialogEstablished: Boolean;
    fDialogParam:       TIdSipDialog;
    fFailure:           Boolean;
    fInviteAgentParam:  TIdSipOutboundInvite;
    fReasonParam:       String;
    fRedirect:          Boolean;
    fResponseParam:     TIdSipResponse;
    fSuccess:           Boolean;

    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TidSipDialog);
    procedure OnFailure(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(InviteAgent: TIdSipOutboundInvite;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property DialogEstablished: Boolean              read fDialogEstablished;
    property DialogParam:       TIdSipDialog         read fDialogParam;
    property Failure:           Boolean              read fFailure;
    property InviteAgentParam:  TIdSipOutboundInvite read fInviteAgentParam;
    property ReasonParam:       String               read fReasonParam;
    property Redirect:          Boolean              read fRedirect;
    property ResponseParam:     TIdSipResponse       read fResponseParam;
    property Success:           Boolean              read fSuccess;
  end;

  TIdSipTestOptionsListener = class(TIdSipMockListener,
                                    IIdSipOptionsListener)
  private
    fOptionsAgentParam: TIdSipOutboundOptions;
    fResponseParam:     TIdSipResponse;
    fResponse:          Boolean;

    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  public
    constructor Create; override;

    property OptionsAgentParam: TIdSipOutboundOptions read fOptionsAgentParam;
    property ResponseParam:     TIdSipResponse        read fResponseParam;
    property Response:          Boolean               read fResponse;
  end;

  TIdSipTestRegistrationListener = class(TIdSipMockListener,
                                         IIdSipRegistrationListener)
  private
    fCurrentBindingsParam: TIdSipContacts;
    fFailure:              Boolean;
    fReasonParam:          String;
    fRegisterAgentParam:   TIdSipOutboundRegistration;
    fSuccess:              Boolean;
  public
    constructor Create; override;

    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);

    property CurrentBindingsParam: TIdSipContacts             read fCurrentBindingsParam;
    property Failure:              Boolean                    read fFailure;
    property ReasonParam:          String                     read fReasonParam;
    property RegisterAgentParam:   TIdSipOutboundRegistration read fRegisterAgentParam;
    property Success:              Boolean                    read fSuccess;
  end;

  TIdSipTestSessionListener = class(TIdSipMockListener,
                                    IIdSipSessionListener)
  private
    fAnswerParam:              TIdSipResponse;
    fEndedSession:             Boolean;
    fEstablishedSession:       Boolean;
    fMimeType:                 String;
    fModifiedSession:          Boolean;
    fModifySession:            Boolean;
    fNewSession:               Boolean;
    fModifyParam:              TIdSipInboundInvite;
    fReasonParam:              String;
    fRedirect:                 Boolean;
    fRemoteSessionDescription: String;
    fSessionParam:             TIdSipSession;
  public
    constructor Create; override;

    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String); virtual;
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Modify: TIdSipInboundInvite);
    procedure OnNewSession(Session: TIdSipSession);

    property AnswerParam:              TIdSipResponse      read fAnswerParam;
    property EndedSession:             Boolean             read fEndedSession;
    property EstablishedSession:       Boolean             read fEstablishedSession;
    property MimeType:                 String              read fMimeType;
    property ModifiedSession:          Boolean             read fModifiedSession;
    property ModifySession:            Boolean             read fModifySession;
    property NewSession:               Boolean             read fNewSession;
    property ModifyParam:              TIdSipInboundInvite read fModifyParam;
    property Redirect:                 Boolean             read fRedirect;
    property RemoteSessionDescription: String              read fRemoteSessionDescription;
    property ReasonParam:              String              read fReasonParam;
    property SessionParam:             TIdSipSession       read fSessionParam;
  end;

  TIdSipTestSessionListenerEndedCounter = class(TIdSipTestSessionListener)
  private
    fEndedNotificationCount: Integer;
  public
    constructor Create; override;

    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String); override;

    property EndedNotificationCount: Integer read fEndedNotificationCount;
  end;

  TIdSipTestTransactionListener = class(TIdSipMockListener,
                                        IIdSipTransactionListener)
  private
    fFailed:                  Boolean;
    fReasonParam:             String;
    fReceivedRequest:         Boolean;
    fReceivedResponse:        Boolean;
    fReceiverParam:           TIdSipTransport;
    fRequestParam:            TIdSipRequest;
    fResponseParam:           TIdSipResponse;
    fTerminated:              Boolean;
    fTransactionParam:        TIdSipTransaction;

    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Receiver: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    constructor Create; override;

    property Failed:                  Boolean           read fFailed;
    property ReasonParam:             String            read fReasonParam;
    property ReceivedRequest:         Boolean           read fReceivedRequest;
    property ReceivedResponse:        Boolean           read fReceivedResponse;
    property ReceiverParam:           TIdSipTransport   read fReceiverParam;
    property RequestParam:            TIdSipRequest     read fRequestParam;
    property ResponseParam:           TIdSipResponse    read fResponseParam;
    property Terminated:              Boolean           read fTerminated;
    property TransactionParam:        TIdSipTransaction read fTransactionParam;
  end;


  TIdSipTestTransportListener = class(TIdSipMockListener,
                                      IIdSipTransportListener)
  private
    fException:        Boolean;
    fExceptionParam:   Exception;
    fMsgParam:         String;
    fReasonParam:      String;
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fReceiverParam:    TIdSipTransport;
    fRejectedMessage:  Boolean;
    fRequestParam:     TIdSipRequest;
    fResponseParam:    TIdSipResponse;

    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
  public
    constructor Create; override;

    property Exception:        Boolean         read fException;
    property ExceptionParam:   Exception       read fExceptionParam;
    property MsgParam:         String          read fMsgParam;
    property ReasonParam:      String          read fReasonParam;
    property ReceivedRequest:  Boolean         read fReceivedRequest;
    property ReceivedResponse: Boolean         read fReceivedResponse;
    property ReceiverParam:    TIdSipTransport read fReceiverParam;
    property RejectedMessage:  Boolean         read fRejectedMessage;
    property RequestParam:     TIdSipRequest   read fRequestParam;
    property ResponseParam:    TIdSipResponse  read fResponseParam;

  end;

  TIdSipTestTransportSendingListener = class(TIdSipMockListener,
                                             IIdSipTransportSendingListener)
  private
    fRequestParam:  TIdSipRequest;
    fResponseParam: TIdSipResponse;
    fSenderParam:   TIdSipTransport;
    fSentRequest:   Boolean;
    fSentResponse:  Boolean;

    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
  public
    constructor Create; override;

    property RequestParam:  TIdSipRequest   read fRequestParam;
    property ResponseParam: TIdSipResponse  read fResponseParam;
    property SenderParam:   TIdSipTransport read fSenderParam;
    property SentRequest:   Boolean         read fSentRequest;
    property SentResponse:  Boolean         read fSentResponse;

  end;

  TIdSipTestTransactionDispatcherListener = class(TIdSipMockListener,
                                                  IIdSipTransactionDispatcherListener)
  private
    fAuthenticationChallenge:   Boolean;
    fChallengeParam:            TIdSipResponse;
    fChallengeResponseBranch:   String;
    fDispatcherParam:           TIdSipTransactionDispatcher;
    fReceivedRequest:           Boolean;
    fReceivedResponse:          Boolean;
    fReceivedUnhandledRequest:  Boolean;
    fReceivedUnhandledResponse: Boolean;
    fReceiverParam:             TIdSipTransport;
    fRequestParam:              TIdSipRequest;
    fResponseParam:             TIdSipResponse;
    fTryAgain:                  Boolean;

    procedure OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                        Challenge: TIdSipResponse;
                                        ChallengeResponse: TIdSipRequest;
                                        var TryAgain: Boolean);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
  public
    constructor Create; override;

    property AuthenticationChallenge:   Boolean                     read fAuthenticationChallenge;
    property ChallengeParam:            TIdSipResponse              read fChallengeParam;
    property ChallengeResponseBranch:   String                      read fChallengeResponseBranch write fChallengeResponseBranch;
    property DispatcherParam:           TIdSipTransactionDispatcher read fDispatcherParam;
    property ReceivedRequest:           Boolean                     read fReceivedRequest;
    property ReceivedResponse:          Boolean                     read fReceivedResponse;
    property ReceivedUnhandledRequest:  Boolean                     read fReceivedUnhandledRequest;
    property ReceivedUnhandledResponse: Boolean                     read fReceivedUnhandledResponse;
    property ReceiverParam:             TIdSipTransport             read fReceiverParam;
    property RequestParam:              TIdSipRequest               read fRequestParam;
    property ResponseParam:             TIdSipResponse              read fResponseParam;
    property TryAgain:                  Boolean                     read fTryAgain write fTryAgain;
  end;

  TIdSipTestUserAgentListener = class(TIdSipMockListener,
                                      IIdSipUserAgentListener)
  private
    fAuthenticationChallenge: Boolean;
    fDroppedUnmatchedMessage: Boolean;
    fInboundCall:             Boolean;
    fPassword:                String;
    fReceiverParam:           TIdSipTransport;
    fResponseParam:           TIdSipResponse;
    fMessageParam:            TIdSipMessage;
    fSessionParam:            TIdSipInboundSession;
    fTryAgain:                Boolean;
    fUserAgentParam:          TIdSipAbstractUserAgent;
    fUsername:                String;

    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean);
    procedure OnDroppedUnmatchedMessage(Message: TIdSipMessage;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
  public
    constructor Create; override;

    property AuthenticationChallenge: Boolean                 read fAuthenticationChallenge;
    property DroppedUnmatchedMessage: Boolean                 read fDroppedUnmatchedMessage;
    property InboundCall:             Boolean                 read fInboundCall;
    property Password:                String                  read fPassword write fPassword;
    property ReceiverParam:           TIdSipTransport         read fReceiverParam;
    property ResponseParam:           TIdSipResponse          read fResponseParam;
    property MessageParam:            TIdSipMessage           read fMessageParam;
    property SessionParam:            TIdSipInboundSession    read fSessionParam;
    property TryAgain:                Boolean                 read fTryAgain write fTryAgain;
    property UserAgentParam:          TIdSipAbstractUserAgent read fUserAgentParam;
    property Username:                String                  read fUsername write fUsername;
  end;

  TIdSipActionFinder = class(TIdSipActionClosure)
  private
    fAction: TIdSipAction;
  public
    procedure Execute(Action: TIdSipAction); override;

    property Action: TIdSipAction read fAction;
  end;

  TIdSipActionSwitch = class(TIdSipActionClosure)
  private
    fExecuted: Boolean;
  public
    constructor Create; 

    procedure Execute(Action: TIdSipAction); override;

    property Executed: Boolean read fExecuted;
  end;

  // constants used in tests
const
  CertPasswd     = 'test';
  DefaultTimeout = 1000;
  RootCert       = '..\etc\cacert.pem';
  ServerCert     = '..\etc\newcert.pem';
  ServerKey      = '..\etc\newkey.pem';

implementation

uses
  IdSipConsts;

//******************************************************************************
//* TIdSipTestResources                                                        *
//******************************************************************************
//* TIdSipTestResources Public methods *****************************************

class function TIdSipTestResources.CreateBasicRequest: TIdSipRequest;
begin
  Result := Self.CreateCommonRequest;
  Result.RequestUri.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Result.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Result.ToHeader.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775';
  Result.From.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Result.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';
end;

class function TIdSipTestResources.CreateBasicResponse: TIdSipResponse;
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateBasicRequest;
  try
    Result := TIdSipResponse.InResponseTo(Request, SIPBusyHere);
    Result.AddHeader(ContactHeaderFull).Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
  finally
    Request.Free;
  end;
end;

class function TIdSipTestResources.CreateLocalLoopRequest: TIdSipRequest;
begin
  Result := Self.CreateCommonRequest;
  Result.RequestUri.Uri := 'sip:franks@127.0.0.1';
  Result.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/TCP 127.0.0.1;branch=z9hG4bK776asdhds';
  Result.ToHeader.Value := 'Wintermute <sip:franks@127.0.0.1>';
  Result.From.Value := 'Case <sip:franks@127.0.0.1>;tag=1928301774';
  Result.AddHeader(ContactHeaderFull).Value := 'sip:franks@127.0.0.1';
end;

class function TIdSipTestResources.CreateLocalLoopResponse: TIdSipResponse;
var
  Request: TIdSipRequest;
begin
  Request := Self.CreateLocalLoopRequest;
  try
    Result := TIdSipResponse.InResponseTo(Request, SIPBusyHere);
  finally
    Request.Free;
  end;
end;

class function TIdSipTestResources.BasicSDP(const Host: String): String;
begin
  Result := 'v=0'#13#10
          + 'o=sc 1106835019 1106835019 IN IP4 ' + Host + #13#10
          + 's=Dummy on hold SDP'#13#10
          + 'c=IN IP4 0.0.0.0'#13#10
          + 'm=audio 65534 RTP/AVP 0'#13#10
          + 'a=rtpmap:0 PCMU/8000'#13#10;
end;

class function TIdSipTestResources.VeryLargeSDP(const Host: String): String;
begin
  Result := 'v=0'#13#10
         + 'o=sc 1106835019 1106835019 IN IP4 ' + Host + #13#10
         + 's=Dummy on hold SDP'#13#10;

  while (Length(Result) < MaximumUDPMessageSize) do
    Result := Result + 'i=Junk session info lies here just to tick off your local SDP parser'#13#10;
end;

//* TIdSipTestResources Private methods ****************************************

class function TIdSipTestResources.CreateCommonRequest: TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  Result.Method := MethodInvite;
  Result.ContentType := 'text/plain';
  Result.Body := 'I am a message. Hear me roar!';
  Result.ContentLength := Length(Result.Body);
  Result.MaxForwards := 70;
  Result.SIPVersion := SipVersion;
  Result.CallID := 'a84b4c76e66710@gw1.leo-ix.org';
  Result.CSeq.Method := Result.Method;
  Result.CSeq.SequenceNo := 314159;
end;

//******************************************************************************
//* TTestCaseSip                                                               *
//******************************************************************************
//* TTestCaseSip Public methods ************************************************

procedure TTestCaseSip.CheckEquals(Expected,
                                   Received: TIdSipURI;
                                   const Msg: String);
begin
  CheckEquals(Expected.URI, Received.URI, Msg);
end;

procedure TTestCaseSip.CheckEquals(Expected,
                                       Received: TIdSipHeadersFilter;
                                       const Msg: String);
var
  I: Cardinal;
begin
  Expected.First;
  Received.First;

  I := 1;
  while Expected.HasNext and Received.HasNext do begin
    CheckEquals(Expected.CurrentHeader.Value,
                Received.CurrentHeader.Value,
                Msg + ': ' + IntToStr(I) + 'st header');
    Expected.Next;
    Received.Next;
    Inc(I);
  end;

  CheckEquals(Expected.Count,
              Received.Count,
              Msg + ': Number of headers');
end;

//******************************************************************************
//* TIdSipExceptionRaisingHeader                                               *
//******************************************************************************
//* TIdSipExceptionRaisingHeader Protected methods *****************************

procedure TIdSipExceptionRaisingHeader.Parse(const Value: String);
begin
  raise EBadHeader.Create(Self.ClassName + '.Parse');
end;

//******************************************************************************
//* TIdSipMockListener                                                         *
//******************************************************************************
//* TIdSipMockListener Public methods ******************************************

constructor TIdSipMockListener.Create;
begin
  inherited Create;

  Self.FailWith       := nil;
  Self.fNetworkFailed := false;
  Self.fReasonParam   := '';
end;

//* TIdSipMockListener Private methods *****************************************

procedure TIdSipMockListener.OnNetworkFailure(Action: TIdSipAction;
                                              const Reason: String);
begin
  Self.fNetworkFailed := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

//******************************************************************************
//* TIdSipTestDataListener                                                     *
//******************************************************************************
//* TIdSipTestDataListener Public methods **************************************

constructor TIdSipTestDataListener.Create;
begin
  inherited Create;

  Self.fNewData    := false;
  Self.fNewUdpData := false;
end;

procedure TIdSipTestDataListener.OnNewData(Data: TIdRTPPayload;
                                           Binding: TIdConnection);
begin
  Self.fNewData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewData');
end;

procedure TIdSipTestDataListener.OnNewUdpData(Data: TStream);
begin
  Self.fNewUdpData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewUdpData');
end;

//******************************************************************************
//* TIdSipTestMessageListener                                                  *
//******************************************************************************
//* TIdSipTestMessageListener Public methods ***********************************

constructor TIdSipTestMessageListener.Create;
begin
  inherited Create;

  Self.fException        := false;
  Self.fMalformedMessage := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
end;

//* TIdSipTestMessageListener Private methods **********************************

procedure TIdSipTestMessageListener.OnException(E: Exception;
                                                const Reason: String);
begin
  Self.fException      := true;
  Self.fExceptionParam := E;
  Self.fReasonParam    := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnException');
end;

procedure TIdSipTestMessageListener.OnMalformedMessage(const Msg: String;
                                                       const Reason: String);
begin
  Self.fMalformedMessage      := true;
  Self.fMalformedMessageParam := Msg;
  Self.fReasonParam           := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnMalformedMessage');
end;

procedure TIdSipTestMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedRequest   := true;
  Self.fRequestParam      := Request;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedResponse  := true;
  Self.fResponseParam     := Response;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

//******************************************************************************
//* TIdSipTestObserver                                                         *
//******************************************************************************
//* TIdSipTestObserver Public methods ******************************************

constructor TIdSipTestObserver.Create;
begin
  inherited Create;

  Self.fChanged := false;
end;

//* TIdSipTestObserver Private methods *****************************************

procedure TIdSipTestObserver.OnChanged(Observed: TObject);
begin
  Self.fChanged := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnChanged');
end;

//******************************************************************************
//* TIdSipTestInboundInviteListener                                            *
//******************************************************************************
//* TIdSipTestInboundInviteListener Public methods *****************************

constructor TIdSipTestInboundInviteListener.Create;
begin
  inherited Create;

  Self.fAckParam         := nil;
  Self.fFailed           := false;
  Self.fInviteAgentParam := nil;
  Self.fSucceeded        := false;
end;

//* TIdSipTestInboundInviteListener Private methods ****************************

procedure TIdSipTestInboundInviteListener.OnFailure(InviteAgent: TIdSipInboundInvite);
begin
  Self.fFailed           := true;
  Self.fInviteAgentParam := InviteAgent;
end;

procedure TIdSipTestInboundInviteListener.OnSuccess(InviteAgent: TIdSipInboundInvite;
                                                    Ack: TIdSipRequest);
begin
  Self.fAckParam         := Ack;
  Self.fInviteAgentParam := InviteAgent;
  Self.fSucceeded        := true;
end;

//******************************************************************************
//* TIdSipTestInviteListener                                                   *
//******************************************************************************
//* TIdSipTestInviteListener Public methods ************************************

constructor TIdSipTestInviteListener.Create;
begin
  inherited Create;

  Self.fDialogEstablished := false;
  Self.fFailure           := false;
  Self.fSuccess           := false;
end;

//* TIdSipTestInviteListener Private methods **********************************

procedure TIdSipTestInviteListener.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                                       NewDialog: TidSipDialog);
begin
  Self.fDialogEstablished := true;
  Self.fInviteAgentParam  := InviteAgent;
  Self.fDialogParam       := NewDialog;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnDialogEstablished');
end;

procedure TIdSipTestInviteListener.OnFailure(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse;
                                             const Reason: String);
begin
  Self.fFailure          := true;
  Self.fInviteAgentParam := InviteAgent;
  Self.fResponseParam    := Response;
  Self.fReasonParam      := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestInviteListener.OnRedirect(InviteAgent: TIdSipOutboundInvite;
                                              Redirect: TIdSipResponse);
begin
  Self.fInviteAgentParam := InviteAgent;
  Self.fRedirect         := true;
  Self.fResponseParam    := Redirect;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirect');
end;

procedure TIdSipTestInviteListener.OnSuccess(InviteAgent: TIdSipOutboundInvite;
                                             Response: TIdSipResponse);
begin
  Self.fInviteAgentParam := InviteAgent;
  Self.fResponseParam     := Response;
  Self.fSuccess           := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestOptionsListener                                                  *
//******************************************************************************
//* TIdSipTestOptionsListener Public methods ***********************************

constructor TIdSipTestOptionsListener.Create;
begin
  inherited Create;

  Self.fResponse := false;
end;

//* TIdSipTestOptionsListener Private methods **********************************

procedure TIdSipTestOptionsListener.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                              Response: TIdSipResponse);
begin
  Self.fOptionsAgentParam := OptionsAgent;
  Self.fResponseParam     := Response;
  Self.fResponse          := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnResponse');
end;

//******************************************************************************
//* TIdSipTestRegistrationListener                                             *
//******************************************************************************
//* TIdSipTestRegistrationListener Public methods ******************************

constructor TIdSipTestRegistrationListener.Create;
begin
  inherited Create;

  Self.fFailure := false;
  Self.fSuccess := false;
end;

//* TIdSipRegistrationListener Private methods *********************************

procedure TIdSipTestRegistrationListener.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                                   CurrentBindings: TIdSipContacts;
                                                   const Reason: String);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fFailure              := true;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fReasonParam          := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFailure');
end;

procedure TIdSipTestRegistrationListener.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                                   CurrentBindings: TIdSipContacts);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fSuccess              := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestSessionListener                                                  *
//******************************************************************************
//* TIdSipTestSessionListener Public methods ***********************************

constructor TIdSipTestSessionListener.Create;
begin
  inherited Create;

  Self.fEndedSession             := false;
  Self.fEstablishedSession       := false;
  Self.fMimeType                 := '';
  Self.fModifiedSession          := false;
  Self.fNewSession               := false;
  Self.fRedirect                 := false;
  Self.fRemoteSessionDescription := '';
end;

procedure TIdSipTestSessionListener.OnRedirect(Action: TIdSipAction;
                                               Redirect: TIdSipResponse);
begin
  Self.fRedirect := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRedirect');
end;

procedure TIdSipTestSessionListener.OnEndedSession(Session: TIdSipSession;
                                                   const Reason: String);
begin
  Self.fEndedSession := true;
  Self.fReasonParam  := Reason;
  Self.fSessionParam := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnEndedSession');
end;

procedure TIdSipTestSessionListener.OnEstablishedSession(Session: TIdSipSession;
                                                         const RemoteSessionDescription: String;
                                                         const MimeType: String);
begin
  Self.fEstablishedSession       := true;
  Self.fSessionParam             := Session;
  Self.fRemoteSessionDescription := RemoteSessionDescription;
  Self.fMimeType                 := MimeType;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnEstablishedSession');
end;

procedure TIdSipTestSessionListener.OnModifiedSession(Session: TIdSipSession;
                                                      Answer: TIdSipResponse);
begin
  Self.fAnswerParam     := Answer;
  Self.fModifiedSession := true;
  Self.fSessionParam    := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnModifiedSession');
end;

procedure TIdSipTestSessionListener.OnModifySession(Modify: TIdSipInboundInvite);
begin
  Self.fModifySession := true;
  Self.fModifyParam   := Modify;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnModifiedSession');
end;

procedure TIdSipTestSessionListener.OnNewSession(Session: TIdSipSession);
begin
  Self.fNewSession   := true;
  Self.fSessionParam := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnNewSession');
end;

//******************************************************************************
//* TIdSipTestSessionListenerEndedCounter                                      *
//******************************************************************************
//* TIdSipTestSessionListenerEndedCounter Public methods ***********************

constructor TIdSipTestSessionListenerEndedCounter.Create;
begin
  inherited Create;

  Self.fEndedNotificationCount := 0;
end;

procedure TIdSipTestSessionListenerEndedCounter.OnEndedSession(Session: TIdSipSession;
                                                               const Reason: String);
begin
  inherited OnEndedSession(Session, Reason);

  Inc(Self.fEndedNotificationCount);
end;

//******************************************************************************
//* TIdSipTestTransactionListener                                              *
//******************************************************************************
//* TIdSipTestTransactionListener Public methods *******************************

constructor TIdSipTestTransactionListener.Create;
begin
  inherited Create;

  Self.fFailed           := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fTerminated       := false;
end;

//* TIdSipTestTransactionListener Private methods ******************************

procedure TIdSipTestTransactionListener.OnFail(Transaction: TIdSipTransaction;
                                               const Reason: String);
begin
  Self.fFailed           := true;
  Self.fReasonParam      := Reason;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnFail');
end;

procedure TIdSipTestTransactionListener.OnReceiveRequest(Request: TIdSipRequest;
                                                         Transaction: TIdSipTransaction;
                                                         Receiver: TIdSipTransport);
begin
  Self.fReceivedRequest  := true;
  Self.fReceiverParam    := Receiver;
  Self.fRequestParam     := Request;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionListener.OnReceiveResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Receiver: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransactionListener.OnTerminated(Transaction: TIdSipTransaction);
begin
  Self.fTerminated       := true;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnTerminated');
end;

//******************************************************************************
//* TIdSipTestTransportListener                                                *
//******************************************************************************
//* TIdSipTestTransportListener Public methods *********************************

constructor TIdSipTestTransportListener.Create;
begin
  inherited Create;

  Self.fException        := false;
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fRejectedMessage  := false;
end;

//* TIdSipTestTransportListener Private methods ********************************

procedure TIdSipTestTransportListener.OnException(E: Exception;
                                                  const Reason: String);
begin
  Self.fException      := true;
  Self.fExceptionParam := E;
  Self.fReasonParam    := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnException');
end;

procedure TIdSipTestTransportListener.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport);
begin
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;
  Self.fReceivedRequest := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport);
begin
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;
  Self.fReceivedResponse := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

procedure TIdSipTestTransportListener.OnRejectedMessage(const Msg: String;
                                                        const Reason: String);
begin
  Self.fMsgParam        := Msg;
  Self.fReasonParam     := Reason;
  Self.fRejectedMessage := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnRejectedMessage');

end;

//******************************************************************************
//* TIdSipTestTransportSendingListener                                         *
//******************************************************************************
//* TIdSipTestTransportSendingListener Public methods **************************

constructor TIdSipTestTransportSendingListener.Create;
begin
  inherited Create;

  Self.fSentRequest      := false;
  Self.fSentResponse     := false;
end;

//* TIdSipTestTransportSendingListener Private methods *************************


procedure TIdSipTestTransportSendingListener.OnSendRequest(Request: TIdSipRequest;
                                                           Sender: TIdSipTransport);
begin
  Self.fRequestParam := Request;
  Self.fSenderParam  := Sender;
  Self.fSentRequest  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendRequest');
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(Response: TIdSipResponse;
                                                            Sender: TIdSipTransport);
begin
  Self.fResponseParam := Response;
  Self.fSenderParam   := Sender;
  Self.fSentResponse  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnSendResponse');
end;

//******************************************************************************
//* TIdSipTestTransactionDispatcherListener                                    *
//******************************************************************************
//* TIdSipTestTransactionDispatcherListener Public methods *********************

constructor TIdSipTestTransactionDispatcherListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge   := false;
  Self.fReceivedRequest           := false;
  Self.fReceivedResponse          := false;
  Self.fReceivedUnhandledRequest  := false;
  Self.fReceivedUnhandledResponse := false;

  // Usually you'd want to re-issue a request that the UAS challenged.
  Self.TryAgain := true;
end;

//* TIdSipTestTransactionDispatcherListener Private methods ********************

procedure TIdSipTestTransactionDispatcherListener.OnAuthenticationChallenge(Dispatcher: TIdSipTransactionDispatcher;
                                                                            Challenge: TIdSipResponse;
                                                                            ChallengeResponse: TIdSipRequest;
                                                                            var TryAgain: Boolean);
begin
  Self.fAuthenticationChallenge := true;
  Self.fDispatcherParam         := Dispatcher;
  Self.fChallengeParam          := Challenge;
  Self.fChallengeResponseBranch := ChallengeResponse.LastHop.Branch;

  // We set the var parameter, not our instance variable!
  TryAgain := Self.TryAgain;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAuthenticationChallenge');
end;

procedure TIdSipTestTransactionDispatcherListener.OnReceiveRequest(Request: TIdSipRequest;
                                                              Receiver: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveRequest');
end;

procedure TIdSipTestTransactionDispatcherListener.OnReceiveResponse(Response: TIdSipResponse;
                                                               Receiver: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnReceiveResponse');
end;

//******************************************************************************
//* TIdSipTestUserAgentListener                                                *
//******************************************************************************
//* TIdSipTestUserAgentListener Public methods *********************************

constructor TIdSipTestUserAgentListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge := false;
  Self.fDroppedUnmatchedMessage := false;
  Self.fInboundCall             := false;
end;

//* TIdSipTestUserAgentListener Private methods ********************************

procedure TIdSipTestUserAgentListener.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                                                Challenge: TIdSipResponse;
                                                                var Username: String;
                                                                var Password: String;
                                                                var TryAgain: Boolean);
begin
  Self.fUserAgentParam          := UserAgent;
  Self.fAuthenticationChallenge := true;
  Self.fResponseParam           := Challenge;

  // We set the var parameter, not our instance variable!
  Password := Self.Password;
  TryAgain := Self.TryAgain;
  Username := Self.Username;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAuthenticationChallenge');
end;

procedure TIdSipTestUserAgentListener.OnDroppedUnmatchedMessage(Message: TIdSipMessage;
                                                                Receiver: TIdSipTransport);
begin
  Self.fDroppedUnmatchedMessage := true;
  Self.fReceiverParam           := Receiver;
  Self.fMessageParam            := Message;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnDroppedUnmatchedMessage');
end;

procedure TIdSipTestUserAgentListener.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.fInboundCall  := true;
  Self.fSessionParam := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnInboundCall');
end;

//******************************************************************************
//* TIdSipActionFinder                                                         *
//******************************************************************************
//* TIdSipActionFinder Public methods ******************************************

procedure TIdSipActionFinder.Execute(Action: TIdSipAction);
begin
  Self.fAction := Action;
end;

//******************************************************************************
//* TIdSipActionSwitch                                                         *
//******************************************************************************
//* TIdSipActionSwitch Public methods ******************************************

constructor TIdSipActionSwitch.Create;
begin
  inherited Create;

  Self.fExecuted := false;
end;

procedure TIdSipActionSwitch.Execute(Action: TIdSipAction);
begin
  Self.fExecuted := true;
end;

end.
