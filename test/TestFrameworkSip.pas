unit TestFrameworkSip;

interface

uses
  IdURI, IdSipInterfacedObject, IdSipMessage, IdSipCore, IdSipTransaction,
  IdSipTransport, TestFrameworkEx;

type
  TTestCaseSip = class(TThreadingTestCase)
    procedure CheckEquals(Expected, Received: TIdURI; Message: String); overload;
  end;

  TIdSipTestMessageListener = class(TIdSipInterfacedObject, IIdSipMessageListener)
  private
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;

    procedure OnReceiveRequest(const Request: TIdSipRequest);
    procedure OnReceiveResponse(const Response: TIdSipResponse);
  public
    constructor Create;

    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
  end;

  TIdSipTestObserver = class(TIdSipInterfacedObject, IIdSipObserver)
  private
    fChanged: Boolean;
  public
    constructor Create;

    procedure OnChanged(const Observed: TObject);

    property Changed: Boolean read fChanged;
  end;

  TIdSipTestSessionListener = class(TIdSipInterfacedObject,
                                    IIdSipSessionListener)
  private
    fEndedSession:       Boolean;
    fEstablishedSession: Boolean;
    fModifiedSession:    Boolean;
    fNewSession:         Boolean;
  public
    constructor Create;

    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewSession(const Session: TIdSipSession);

    property EndedSession:       Boolean read fEndedSession;
    property EstablishedSession: Boolean read fEstablishedSession;
    property ModifiedSession:    Boolean read fModifiedSession;
    property NewSession:         Boolean read fNewSession;
  end;

  TIdSipTestTransactionListener = class(TIdSipInterfacedObject, IIdSipTransactionListener)
  private
    fFailReason:       String;
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;
    fTerminated:       Boolean;

    procedure OnFail(const Transaction: TIdSipTransaction;
                     const Reason: String);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transaction: TIdSipTransaction;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transaction: TIdSipTransaction;
                                const Transport: TIdSipTransport);
    procedure OnTerminated(const Transaction: TIdSipTransaction);
  public
    constructor Create;

    property FailReason:       String  read fFailReason;
    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
    property Terminated:       Boolean read fTerminated;
  end;

  TIdSipTestTransportListener = class(TIdSipInterfacedObject,
                                      IIdSipTransportListener)
  private
    fReceivedRequest:  Boolean;
    fReceivedResponse: Boolean;

    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
  public
    constructor Create;

    property ReceivedRequest:  Boolean read fReceivedRequest;
    property ReceivedResponse: Boolean read fReceivedResponse;
  end;

  TIdSipTestTransportSendingListener = class(TIdSipInterfacedObject,
                                             IIdSipTransportSendingListener)
  private
    fSentRequest:      Boolean;
    fSentResponse:     Boolean;

    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
  public
    constructor Create;

    property SentRequest:  Boolean read fSentRequest;
    property SentResponse: Boolean read fSentResponse;
  end;

implementation

//******************************************************************************
//* TTestCaseSip                                                               *
//******************************************************************************
//* TTestCaseSip Public methods ************************************************

procedure TTestCaseSip.CheckEquals(Expected, Received: TIdURI; Message: String);
begin
  CheckEquals(Expected.GetFullURI, Received.GetFullURI, Message);
end;

//******************************************************************************
//* TIdSipTestMessageListener                                                  *
//******************************************************************************
//* TIdSipTestMessageListener Public methods ***********************************

constructor TIdSipTestMessageListener.Create;
begin
  inherited Create;

  Self.fReceivedRequest := false;
  Self.fReceivedResponse := false;
end;

//* TIdSipTestMessageListener Private methods **********************************

procedure TIdSipTestMessageListener.OnReceiveRequest(const Request: TIdSipRequest);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestMessageListener.OnReceiveResponse(const Response: TIdSipResponse);
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

procedure TIdSipTestObserver.OnChanged(const Observed: TObject);
begin
  Self.fChanged := true;
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

procedure TIdSipTestSessionListener.OnEndedSession(const Session: TIdSipSession);
begin
  Self.fEndedSession := true;
end;

procedure TIdSipTestSessionListener.OnEstablishedSession(const Session: TIdSipSession);
begin
  Self.fEstablishedSession := true;
end;

procedure TIdSipTestSessionListener.OnModifiedSession(const Session: TIdSipSession;
                                                      const Invite: TIdSipRequest);
begin
  Self.fModifiedSession := true;
end;

procedure TIdSipTestSessionListener.OnNewSession(const Session: TIdSipSession);
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

procedure TIdSipTestTransactionListener.OnFail(const Transaction: TIdSipTransaction;
                                               const Reason: String);
begin
  Self.fFailReason := Reason;
end;

procedure TIdSipTestTransactionListener.OnReceiveRequest(const Request: TIdSipRequest;
                                                         const Transaction: TIdSipTransaction;
                                                         const Transport: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestTransactionListener.OnReceiveResponse(const Response: TIdSipResponse;
                                                          const Transaction: TIdSipTransaction;
                                                          const Transport: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
end;

procedure TIdSipTestTransactionListener.OnTerminated(const Transaction: TIdSipTransaction);
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
end;

//* TIdSipTestTransportListener Private methods ********************************

procedure TIdSipTestTransportListener.OnReceiveRequest(const Request: TIdSipRequest;
                                                       const Transport: TIdSipTransport);
begin
  Self.fReceivedRequest := true;
end;

procedure TIdSipTestTransportListener.OnReceiveResponse(const Response: TIdSipResponse;
                                                        const Transport: TIdSipTransport);
begin
  Self.fReceivedResponse := true;
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


procedure TIdSipTestTransportSendingListener.OnSendRequest(const Request: TIdSipRequest;
                                                           const Transport: TIdSipTransport);
begin
  Self.fSentRequest := true;
end;

procedure TIdSipTestTransportSendingListener.OnSendResponse(const Response: TIdSipResponse;
                                                            const Transport: TIdSipTransport);
begin
  Self.fSentResponse := true;
end;

end.
