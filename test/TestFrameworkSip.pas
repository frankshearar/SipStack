unit TestFrameworkSip;

interface

uses
  IdURI, IdSipInterfacedObject, IdSipMessage, TestFrameworkEx;

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

end.
