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
  IdSipCore, IdSipTcpClient, IdSipTcpServer, IdSipTransaction, IdSipTransport,
  IdSocketHandle, SysUtils, TestFrameworkEx;

type
  TIdSipTestResources = class(TObject)
  private
    class function CreateCommonRequest: TIdSipRequest;
  public
    class function CreateBasicRequest: TIdSipRequest;
    class function CreateBasicResponse: TIdSipResponse;
    class function CreateLocalLoopRequest: TIdSipRequest;
    class function CreateLocalLoopResponse: TIdSipResponse;
  end;

  TTestCaseSip = class(TThreadingTestCase)
  public
    procedure CheckEquals(Expected, Received: TIdSipURI; Message: String); overload;
  end;

  TIdSipExceptionRaisingHeader = class(TIdSipHeader)
  protected
    procedure Parse(const Value: String); override;
  end;

  TIdSipMockListener = class(TIdInterfacedObject)
  private
    fFailWith: ExceptClass;
  public
    constructor Create; virtual;

    property FailWith: ExceptClass read fFailWith write fFailWith;
  end;

  TIdSipTestActionListener = class(TIdSipMockListener,
                               IIdSipActionListener)
  private
    fAuthenticationChallenge: Boolean;
    fPassword:                String;
  public
    constructor Create; override;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse;
                                        var Password: String);

    property AuthenticationChallenge: Boolean read fAuthenticationChallenge;
    property Password:                String  read fPassword write fPassword;
  end;

  TIdSipTestDataListener = class(TIdSipMockListener,
                                 IIdRtpDataListener)
  private
    fNewData:    Boolean;
    fNewUdpData: Boolean;
  public
    constructor Create; override;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
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
    fReasonParam:           String;
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
    property ReasonParam:           String                   read fReasonParam;
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

  TIdSipTestOptionsListener = class(TIdSipTestActionListener,
                                    IIdSipOptionsListener)
  private
    fFailure:           Boolean;
    fOptionsAgentParam: TIdSipOutboundOptions;
    fReasonParam:       String;
    fResponseParam:     TIdSipResponse;
    fSuccess:           Boolean;

    procedure OnFailure(OptionsAgent: TIdSipOutboundOptions;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnSuccess(OptionsAgent: TIdSipOutboundOptions;
                        Response: TIdSipResponse);
  public
    constructor Create; override;

    property Failure:           Boolean               read fFailure;
    property OptionsAgentParam: TIdSipOutboundOptions read fOptionsAgentParam;
    property ReasonParam:       String                read fReasonParam;
    property ResponseParam:     TIdSipResponse        read fResponseParam;
    property Success:           Boolean               read fSuccess;
  end;

  TIdSipTestRegistrationListener = class(TIdSipTestActionListener,
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
    fAuthenticationChallenge: Boolean;
    fEndedSession:            Boolean;
    fEstablishedSession:      Boolean;
    fModifiedSession:         Boolean;
    fNewSession:              Boolean;
  public
    constructor Create; override;

    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse;
                                        var Password: String);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnNewSession(Session: TIdSipSession);

    property AuthenticationChallenge: Boolean read fAuthenticationChallenge;
    property EndedSession:            Boolean read fEndedSession;
    property EstablishedSession:      Boolean read fEstablishedSession;
    property ModifiedSession:         Boolean read fModifiedSession;
    property NewSession:              Boolean read fNewSession;
  end;

  TIdSipTestTransactionListener = class(TIdSipMockListener,
                                        IIdSipTransactionListener)
  private
    fFailed:           Boolean;
    fReasonParam:      String;
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fReceiverParam:    TIdSipTransport;
    fRequestParam:     TIdSipRequest;
    fResponseParam:    TIdSipResponse;
    fTerminated:       Boolean;
    fTransactionParam: TIdSipTransaction;

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

    property Failed:           Boolean           read fFailed;
    property ReasonParam:      String            read fReasonParam;
    property ReceivedRequest:  Boolean           read fReceivedRequest;
    property ReceivedResponse: Boolean           read fReceivedResponse;
    property ReceiverParam:    TIdSipTransport   read fReceiverParam;
    property RequestParam:     TIdSipRequest     read fRequestParam;
    property ResponseParam:    TIdSipResponse    read fResponseParam;
    property Terminated:       Boolean           read fTerminated;
    property TransactionParam: TIdSipTransaction read fTransactionParam;
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

  TIdSipTestUnhandledMessageListener = class(TIdSipMockListener,
                                             IIdSipUnhandledMessageListener)
  private
    fReceivedRequest:           Boolean;
    fReceivedResponse:          Boolean;
    fReceivedUnhandledRequest:  Boolean;
    fReceivedUnhandledResponse: Boolean;
    fReceiverParam:             TIdSipTransport;
    fRequestParam:              TIdSipRequest;
    fResponseParam:             TIdSipResponse;


    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
  public
    constructor Create; override;

    property ReceivedRequest:           Boolean         read fReceivedRequest;
    property ReceivedResponse:          Boolean         read fReceivedResponse;
    property ReceivedUnhandledRequest:  Boolean         read fReceivedUnhandledRequest;
    property ReceivedUnhandledResponse: Boolean         read fReceivedUnhandledResponse;
    property ReceiverParam:             TIdSipTransport read fReceiverParam;
    property RequestParam:              TIdSipRequest   read fRequestParam;
    property ResponseParam:             TIdSipResponse   read fResponseParam;
  end;

  TIdSipTestUserAgentListener = class(TIdSipMockListener,
                                      IIdSipUserAgentListener)
  private
    fDroppedUnmatchedResponse: Boolean;
    fInboundCall:              Boolean;
    fReceiverParam:            TIdSipTransport;
    fResponseParam:            TIdSipResponse;
    fSessionParam:             TIdSipInboundSession;

    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnInboundCall(Session: TIdSipInboundSession);
  public
    constructor Create; override;

    property DroppedUnmatchedResponse: Boolean              read fDroppedUnmatchedResponse;
    property InboundCall:              Boolean              read fInboundCall;
    property ReceiverParam:            TIdSipTransport      read fReceiverParam;
    property ResponseParam:            TIdSipResponse       read fResponseParam;
    property SessionParam:             TIdSipInboundSession read fSessionParam;
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

procedure TTestCaseSip.CheckEquals(Expected, Received: TIdSipURI; Message: String);
begin
  CheckEquals(Expected.URI, Received.URI, Message);
end;

//******************************************************************************
//* TIdSipExceptionRaisingHeader                                               *
//******************************************************************************
//* TIdSipExceptionRaisingHeader Protected methods *****************************

procedure TIdSipExceptionRaisingHeader.Parse(const Value: String);
begin
  raise EBadHeader.Create('TIdSipExceptionRaisingHeader.Parse');
end;

//******************************************************************************
//* TIdSipMockListener                                                         *
//******************************************************************************
//* TIdSipMockListener Public methods ******************************************

constructor TIdSipMockListener.Create;
begin
  Self.FailWith := nil;
end;

//******************************************************************************
//* TIdSipTestActionListener                                                   *
//******************************************************************************
//* TIdSipTestActionListener Public methods ************************************

constructor TIdSipTestActionListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge := false;
  Self.Password                 := '';
end;

procedure TIdSipTestActionListener.OnAuthenticationChallenge(Action: TIdSipAction;
                                                         Response: TIdSipResponse;
                                                         var Password: String);
begin
  Self.fAuthenticationChallenge := true;
  Password := Self.Password;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create(Self.ClassName + '.OnAuthenticationChallenge');
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
                                           Binding: TIdSocketHandle);
begin
  Self.fNewData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestDataListener.OnNewData');
end;

procedure TIdSipTestDataListener.OnNewUdpData(Data: TStream);
begin
  Self.fNewUdpData := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestDataListener.OnNewUdpData');
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
    raise Self.FailWith.Create('TIdSipTestMessageListener.OnException');
end;

procedure TIdSipTestMessageListener.OnMalformedMessage(const Msg: String;
                                                       const Reason: String);
begin
  Self.fMalformedMessage      := true;
  Self.fMalformedMessageParam := Msg;
  Self.fReasonParam           := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestMessageListener.OnMalformedMessage');
end;

procedure TIdSipTestMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedRequest   := true;
  Self.fRequestParam      := Request;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestMessageListener.OnReceiveRequest');
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedResponse  := true;
  Self.fResponseParam     := Response;
  Self.fReceivedFromParam := ReceivedFrom;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestMessageListener.OnReceiveResponse');
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
    raise Self.FailWith.Create('TIdSipTestObserver.OnChanged');
end;

//******************************************************************************
//* TIdSipTestOptionsListener                                                  *
//******************************************************************************
//* TIdSipTestOptionsListener Public methods ***********************************

constructor TIdSipTestOptionsListener.Create;
begin
  inherited Create;

  Self.fFailure := false;
  Self.fSuccess := false;
end;

//* TIdSipTestOptionsListener Private methods **********************************

procedure TIdSipTestOptionsListener.OnFailure(OptionsAgent: TIdSipOutboundOptions;
                                              Response: TIdSipResponse;
                                              const Reason: String);
begin
  Self.fFailure           := true;
  Self.fOptionsAgentParam := OptionsAgent;
  Self.fResponseParam     := Response;
  Self.fReasonParam       := Reason;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestOptionsListener.OnFailure');
end;

procedure TIdSipTestOptionsListener.OnSuccess(OptionsAgent: TIdSipOutboundOptions;
                                              Response: TIdSipResponse);
begin
  Self.fOptionsAgentParam := OptionsAgent;
  Self.fResponseParam     := Response;
  Self.fSuccess           := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestOptionsListener.OnSuccess');
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
    raise Self.FailWith.Create('TIdSipTestRegistrationListener.OnFailure');
end;

procedure TIdSipTestRegistrationListener.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                                   CurrentBindings: TIdSipContacts);
begin
  Self.fCurrentBindingsParam := CurrentBindings;
  Self.fRegisterAgentParam   := RegisterAgent;
  Self.fSuccess              := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestRegistrationListener.OnSuccess');
end;

//******************************************************************************
//* TIdSipTestSessionListener                                                  *
//******************************************************************************
//* TIdSipTestSessionListener Public methods ***********************************

constructor TIdSipTestSessionListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge := true;
  Self.fEndedSession            := false;
  Self.fEstablishedSession      := false;
  Self.fModifiedSession         := false;
  Self.fNewSession              := false;
end;

procedure TIdSipTestSessionListener.OnAuthenticationChallenge(Action: TIdSipAction;
                                                              Response: TIdSipResponse;
                                                              var Password: String);
begin
  Self.fAuthenticationChallenge := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestSessionListener.OnAuthenticationChallenge');
end;

procedure TIdSipTestSessionListener.OnEndedSession(Session: TIdSipSession;
                                                   const Reason: String);
begin
  Self.fEndedSession := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestSessionListener.OnEndedSession');
end;

procedure TIdSipTestSessionListener.OnEstablishedSession(Session: TIdSipSession);
begin
  Self.fEstablishedSession := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestSessionListener.OnEstablishedSession');
end;

procedure TIdSipTestSessionListener.OnModifiedSession(Session: TIdSipSession;
                                                      Invite: TIdSipRequest);
begin
  Self.fModifiedSession := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestSessionListener.OnModifiedSession');
end;

procedure TIdSipTestSessionListener.OnNewSession(Session: TIdSipSession);
begin
  Self.fNewSession := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestSessionListener.OnNewSession');
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
    raise Self.FailWith.Create('TIdSipTestTransactionListener.OnFail');
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
    raise Self.FailWith.Create('TIdSipTestTransactionListener.OnReceiveRequest');
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
    raise Self.FailWith.Create('TIdSipTestTransactionListener.OnReceiveResponse');
end;

procedure TIdSipTestTransactionListener.OnTerminated(Transaction: TIdSipTransaction);
begin
  Self.fTerminated       := true;
  Self.fTransactionParam := Transaction;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestTransactionListener.OnTerminated');
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
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnException');
end;

procedure TIdSipTestTransportListener.OnReceiveRequest(Request: TIdSipRequest;
                                                       Receiver: TIdSipTransport);
begin
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;
  Self.fReceivedRequest := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnReceiveRequest');
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(Response: TIdSipResponse;
                                                        Receiver: TIdSipTransport);
begin
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;
  Self.fReceivedResponse := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnReceiveResponse');
end;

procedure TIdSipTestTransportListener.OnRejectedMessage(const Msg: String;
                                                        const Reason: String);
begin
  Self.fMsgParam        := Msg;
  Self.fReasonParam     := Reason;
  Self.fRejectedMessage := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnRejectedMessage');

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
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnSendRequest');
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(Response: TIdSipResponse;
                                                            Sender: TIdSipTransport);
begin
  Self.fResponseParam := Response;
  Self.fSenderParam   := Sender;
  Self.fSentResponse  := true;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestTransportListener.OnSendResponse');
end;

//******************************************************************************
//* TIdSipTestUnhandledMessageListener                                         *
//******************************************************************************
//* TIdSipTestUnhandledMessageListener Public methods **************************

constructor TIdSipTestUnhandledMessageListener.Create;
begin
  inherited Create;

  Self.fReceivedRequest           := false;
  Self.fReceivedResponse          := false;
  Self.fReceivedUnhandledRequest  := false;
  Self.fReceivedUnhandledResponse := false;
end;

//* TIdSipTestUnhandledMessageListener Private methods *************************

procedure TIdSipTestUnhandledMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                              Receiver: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
  Self.fReceiverParam   := Receiver;
  Self.fRequestParam    := Request;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestUnhandledMessageListener.OnReceiveRequest');
end;

procedure TIdSipTestUnhandledMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                               Receiver: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
  Self.fReceiverParam    := Receiver;
  Self.fResponseParam    := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestUnhandledMessageListener.OnReceiveResponse');
end;

//******************************************************************************
//* TIdSipTestUserAgentListener                                                *
//******************************************************************************
//* TIdSipTestUserAgentListener Public methods *********************************

constructor TIdSipTestUserAgentListener.Create;
begin
  inherited Create;

  Self.fDroppedUnmatchedResponse := false;
  Self.fInboundCall              := false;
end;

//* TIdSipTestUserAgentListener Private methods ********************************

procedure TIdSipTestUserAgentListener.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                                 Receiver: TIdSipTransport);
begin
  Self.fDroppedUnmatchedResponse := true;
  Self.fReceiverParam            := Receiver;
  Self.fResponseParam            := Response;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestUnhandledMessageListener.OnDroppedUnmatchedResponse');
end;

procedure TIdSipTestUserAgentListener.OnInboundCall(Session: TIdSipInboundSession);
begin
  Self.fInboundCall  := true;
  Self.fSessionParam := Session;

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipTestUnhandledMessageListener.OnInboundCall');
end;

end.
