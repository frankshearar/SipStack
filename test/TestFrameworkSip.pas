unit TestFrameworkSip;

interface

uses
  Classes, IdInterfacedObject, IdRTP, IdSdp, IdSipHeaders, IdSipMessage,
  IdSipCore, IdSipTcpClient, IdSipTcpServer, IdSipTransaction, IdSipTransport,
  IdSocketHandle, TestFrameworkEx;

type
  TTestCaseSip = class(TThreadingTestCase)
    procedure CheckEquals(Expected, Received: TIdSipURI; Message: String); overload;
  end;

  TIdSipTestDataListener = class(TIdInterfacedObject,
                                 IIdRtpDataListener)
  private
    fNewData:    Boolean;
    fNewUdpData: Boolean;
  public
    constructor Create;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure OnNewUdpData(Data: TStream);

    property NewData:    Boolean read fNewData;
    property NewUdpData: Boolean read fNewUdpData;
  end;

  TIdSipTestMessageListener = class(TIdInterfacedObject,
                                    IIdSipMessageListener)
  private
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;

    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  public
    constructor Create;

    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
  end;

  TIdSipTestObserver = class(TIdInterfacedObject, IIdSipObserver)
  private
    fChanged: Boolean;
  public
    constructor Create;

    procedure OnChanged(Observed: TObject);

    property Changed: Boolean read fChanged;
  end;

  TIdSipTestRegistrationListener = class(TIdInterfacedObject,
                                         IIdSipRegistrationListener)
  private
    fAuthenticationChallenge: Boolean;
    fFailure:                 Boolean;
    fSuccess:                 Boolean;
  public
    constructor Create;

    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);

    property AuthenticationChallenge: Boolean read fAuthenticationChallenge;
    property Failure:                 Boolean read fSuccess;
    property Success:                 Boolean read fSuccess;
  end;

  TIdSipTestSessionListener = class(TIdInterfacedObject,
                                    IIdSipSessionListener)
  private
    fEndedSession:       Boolean;
    fEstablishedSession: Boolean;
    fModifiedSession:    Boolean;
    fNewSession:         Boolean;
  public
    constructor Create;

    procedure OnEndedSession(Session: TIdSipSession);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnNewSession(Session: TIdSipSession);

    property EndedSession:       Boolean read fEndedSession;
    property EstablishedSession: Boolean read fEstablishedSession;
    property ModifiedSession:    Boolean read fModifiedSession;
    property NewSession:         Boolean read fNewSession;
  end;

  TIdSipTestTransactionListener = class(TIdInterfacedObject,
                                        IIdSipTransactionListener)
  private
    fFailReason:       String;
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fTerminated:       Boolean;

    procedure OnFail(Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transaction: TIdSipTransaction;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transaction: TIdSipTransaction;
                                Transport: TIdSipTransport);
    procedure OnTerminated(Transaction: TIdSipTransaction);
  public
    constructor Create;

    property FailReason:       String  read fFailReason;
    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
    property Terminated:       Boolean read fTerminated;
  end;

  TIdSipTestTransportListener = class(TIdInterfacedObject,
                                      IIdSipTransportListener)
  private
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fRejectedMessage:  Boolean;

    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transport: TIdSipTransport);
    procedure OnRejectedMessage(Message: TIdSipMessage;
                                const Reason: String);
  public
    constructor Create;

    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
    property RejectedMessage:  Boolean read fRejectedMessage;
  end;

  TIdSipTestTransportSendingListener = class(TIdInterfacedObject,
                                             IIdSipTransportSendingListener)
  private
    fSentRequest:  Boolean;
    fSentResponse: Boolean;

    procedure OnSendRequest(Request: TIdSipRequest;
                            Transport: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Transport: TIdSipTransport);
  public
    constructor Create;

    property SentRequest:  Boolean read fSentRequest;
    property SentResponse: Boolean read fSentResponse;
  end;

  TIdSipTestUnhandledMessageListener = class(TIdInterfacedObject,
                                             IIdSipUnhandledMessageListener)
  private
    fReceivedUnhandledRequest:  Boolean;
    fReceivedUnhandledResponse: Boolean;

    procedure OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                        Transaction: TIdSipTransaction;
                                        Receiver: TIdSipTransport);
    procedure OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                         Transaction: TIdSipTransaction;
                                         Receiver: TIdSipTransport);
  public
    constructor Create;

    property ReceivedUnhandledRequest:  Boolean read fReceivedUnhandledRequest;
    property ReceivedUnhandledResponse: Boolean read fReceivedUnhandledResponse;
  end;

  TIdSipTestUserAgentListener = class(TIdInterfacedObject,
                                      IIdSipUserAgentListener)
  private
    fInboundCall: Boolean;

    procedure OnInboundCall(Session: TIdSipSession);
  public
    constructor Create;

    property InboundCall: Boolean read fInboundCall write fInboundCall;
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
  SysUtils;

//******************************************************************************
//* TTestCaseSip                                                               *
//******************************************************************************
//* TTestCaseSip Public methods ************************************************

procedure TTestCaseSip.CheckEquals(Expected, Received: TIdSipURI; Message: String);
begin
  CheckEquals(Expected.URI, Received.URI, Message);
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
end;

procedure TIdSipTestDataListener.OnNewUdpData(Data: TStream);
begin
  Self.fNewUdpData := true;
end;

//******************************************************************************
//* TIdSipTestMessageListener                                                  *
//******************************************************************************
//* TIdSipTestMessageListener Public methods ***********************************

constructor TIdSipTestMessageListener.Create;
begin
  inherited Create;

  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
end;

//* TIdSipTestMessageListener Private methods **********************************

procedure TIdSipTestMessageListener.OnReceiveRequest(Request: TIdSipRequest;
                                                     ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(Response: TIdSipResponse;
                                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.fReceivedResponse := true;
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

procedure TIdSipTestObserver.OnChanged(Observed: TObject);
begin
  Self.fChanged := true;
end;

//******************************************************************************
//* TIdSipTestRegistrationListener                                             *
//******************************************************************************
//* TIdSipTestRegistrationListener Public methods ******************************

constructor TIdSipTestRegistrationListener.Create;
begin
  inherited Create;

  Self.fAuthenticationChallenge := false;
  Self.fFailure                 := false;
  Self.fSuccess                 := false;
end;

procedure TIdSipTestRegistrationListener.OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                                                   Response: TIdSipResponse);
begin
  Self.fAuthenticationChallenge := true;
end;

procedure TIdSipTestRegistrationListener.OnFailure(RegisterAgent: TIdSipRegistration;
                                                   CurrentBindings: TIdSipContacts;
                                                   const Reason: String);
begin
  Self.fFailure := true;
end;

procedure TIdSipTestRegistrationListener.OnSuccess(RegisterAgent: TIdSipRegistration;
                                                   CurrentBindings: TIdSipContacts);
begin
  Self.fSuccess := true;
end;


//******************************************************************************
//* TIdSipTestSessionListener                                                  *
//******************************************************************************
//* TIdSipTestSessionListener Public methods ***********************************

constructor TIdSipTestSessionListener.Create;
begin
  inherited Create;

  Self.fEndedSession       := false;
  Self.fEstablishedSession := false;
  Self.fModifiedSession    := false;
  Self.fNewSession         := false;
end;

procedure TIdSipTestSessionListener.OnEndedSession(Session: TIdSipSession);
begin
  Self.fEndedSession := true;
end;

procedure TIdSipTestSessionListener.OnEstablishedSession(Session: TIdSipSession);
begin
  Self.fEstablishedSession := true;
end;

procedure TIdSipTestSessionListener.OnModifiedSession(Session: TIdSipSession;
                                                      Invite: TIdSipRequest);
begin
  Self.fModifiedSession := true;
end;

procedure TIdSipTestSessionListener.OnNewSession(Session: TIdSipSession);
begin
  Self.fNewSession := true;
end;

//******************************************************************************
//* TIdSipTestTransactionListener                                              *
//******************************************************************************
//* TIdSipTestTransactionListener Public methods *******************************

constructor TIdSipTestTransactionListener.Create;
begin
  inherited Create;

  Self.fFailReason       := '';
  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fTerminated       := false;
end;

//* TIdSipTestTransactionListener Private methods ******************************

procedure TIdSipTestTransactionListener.OnFail(Transaction: TIdSipTransaction;
                                               const Reason: String);
begin
  Self.fFailReason := Reason;
end;

procedure TIdSipTestTransactionListener.OnReceiveRequest(Request: TIdSipRequest;
                                                         Transaction: TIdSipTransaction;
                                                         Transport: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestTransactionListener.OnReceiveResponse(Response: TIdSipResponse;
                                                          Transaction: TIdSipTransaction;
                                                          Transport: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
end;

procedure TIdSipTestTransactionListener.OnTerminated(Transaction: TIdSipTransaction);
begin
  Self.fTerminated := true;
end;

//******************************************************************************
//* TIdSipTestTransportListener                                                *
//******************************************************************************
//* TIdSipTestTransportListener Public methods *********************************

constructor TIdSipTestTransportListener.Create;
begin
  inherited Create;

  Self.fReceivedRequest  := false;
  Self.fReceivedResponse := false;
  Self.fRejectedMessage  := false;
end;

//* TIdSipTestTransportListener Private methods ********************************

procedure TIdSipTestTransportListener.OnReceiveRequest(Request: TIdSipRequest;
                                                       Transport: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(Response: TIdSipResponse;
                                                        Transport: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
end;

procedure TIdSipTestTransportListener.OnRejectedMessage(Message: TIdSipMessage;
                                                        const Reason: String);
begin
  Self.fRejectedMessage := true;
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
                                                           Transport: TIdSipTransport);
begin
  Self.fSentRequest := true;
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(Response: TIdSipResponse;
                                                            Transport: TIdSipTransport);
begin
  Self.fSentResponse := true;
end;

//******************************************************************************
//* TIdSipTestUnhandledMessageListener                                         *
//******************************************************************************
//* TIdSipTestUnhandledMessageListener Public methods **************************

constructor TIdSipTestUnhandledMessageListener.Create;
begin
  inherited Create;

  fReceivedUnhandledRequest  := false;
  fReceivedUnhandledResponse := false;
end;

//* TIdSipTestUnhandledMessageListener Private methods *************************

procedure TIdSipTestUnhandledMessageListener.OnReceiveUnhandledRequest(Request: TIdSipRequest;
                                                                       Transaction: TIdSipTransaction;
                                                                       Receiver: TIdSipTransport);
begin
  fReceivedUnhandledRequest := true;
end;

procedure TIdSipTestUnhandledMessageListener.OnReceiveUnhandledResponse(Response: TIdSipResponse;
                                                                        Transaction: TIdSipTransaction;
                                                                        Receiver: TIdSipTransport);
begin
  fReceivedUnhandledResponse := true;
end;

//******************************************************************************
//* TIdSipTestUserAgentListener                                                *
//******************************************************************************
//* TIdSipTestUserAgentListener Public methods *********************************

constructor TIdSipTestUserAgentListener.Create;
begin
  inherited Create;

  fInboundCall := false;
end;

//* TIdSipTestUserAgentListener Private methods ********************************

procedure TIdSipTestUserAgentListener.OnInboundCall(Session: TIdSipSession);
begin
  fInboundCall := true;
end;

end.
