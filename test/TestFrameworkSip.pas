unit TestFrameworkSip;

interface

uses
  IdURI, IdSipInterfacedObject, IdSipMessage, IdSipTransaction, IdSipTransport,
  TestFrameworkEx;

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

  TIdSipTestTransportListener = class(TIdSipInterfacedObject, IIdSipTransportListener)
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


end.
