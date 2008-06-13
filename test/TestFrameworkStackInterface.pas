{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkStackInterface;

interface

uses
  Contnrs, Forms, IdConnectionBindings, IdInterfacedObject, IdSipMessage,
  IdSipMockTransport, IdSipStackInterface, IdSipTransport, IdTimerQueue,
  TestFramework, TestFrameworkSip, SysUtils;

type
  TIdDataList = class(TIdInterfacedObject)
  private
    DataList: TObjectList; // Holds all the data received from the stack

    procedure CheckEquals(Expected, Actual: Cardinal; Msg: String);
    procedure Fail(Msg: String);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddNotification(Data: TIdEventData); virtual;
    procedure CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
    procedure CheckNotificationsReceived(EventType: TIdEventDataClass; ExpectedCount: Cardinal; Msg: String);
    function  EventAt(Index: Integer): TIdEventData;
    function  LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
    function  SecondLastEventData: TIdEventData;
    function  ThirdLastEventData: TIdEventData;
  end;

  TIdWindowAttachedDataList = class(TIdDataList,
                                    IIdSipStackInterface)
  private
    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);
  public
    procedure AddNotification(Data: TIdEventData); override;
  end;

  TTransportChecking = class(TIdInterfacedObject,
                             IIdSipTransportListener,
                             IIdSipTransportSendingListener)
  private
    Acks:          TIdSipRequestList;
    Requests:      TIdSipRequestList;
    Responses:     TIdSipResponseList;
    RequestCount:  Integer;
    ResponseCount: Integer;

    procedure Check(Condition: Boolean; Msg: String);
    procedure OnException(FailedMessage: TIdSipMessage;
                          E: Exception;
                          const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String;
                                Source: TIdConnectionBindings);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdConnectionBindings);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure CheckNoRequestSent(Msg: String);
    procedure CheckNoResponseSent(Msg: String);
    procedure CheckRequestSent(Msg: String);
    procedure CheckResponseSent(Msg: String);
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    function  SentRequestCount: Integer;
    function  SentResponseCount: Integer;
  end;

  // When writing tests for the stack interface, remember that the stack runs in
  // a separate thread. All the methods (that don't create Actions) of the
  // StackInterface use TIdWaits to schedule events within the stack thread.
  // Thus, when you invoke these methods (like Send, AnswerCall, RejectCall,
  // etc.), you have to trigger the newly-scheduled events by, for instance,
  //
  //    Self.TimerQueue.TriggerAllEventsOfType(TIdSipActionSendWait);
  //
  // The same applies for notifications: the StackWindow sends us notifications
  // like CM_CALL_REQUEST_NOTIFY, and you have to process these notifications
  // (by invoking Application.ProcessMessages) before you can inspect what the
  // stack does with these notification, or how it presents them. This means
  // that if you're establishing a call and you receive a 200 OK, you must call
  // Application.ProcessMessages before the test can know about the response.
  TStackInterfaceTestCase = class(TTestCase)
  private
    DataList:       TIdWindowAttachedDataList;
    fMockTransport: TIdSipMockTransport;
    TransportTest:  TTransportChecking;

    procedure SetMockTransport(Value: TIdSipMockTransport);
  protected
    TimerQueue: TIdDebugTimerQueue;
    UI:         TCustomForm;

    procedure CheckNoRequestSent(Msg: String);
    procedure CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
    procedure CheckNotificationsReceived(EventType: TIdEventDataClass; ExpectedCount: Cardinal; Msg: String);
    procedure CheckRequestSent(Msg: String);
    procedure CheckResponseSent(Msg: String);
    function  EventAt(Index: Integer): TIdEventData;
    function  LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
    function  LastSentRequest: TIdSipRequest;
    function  LastSentResponse: TIdSipResponse;
    procedure MarkSentRequestCount;
    procedure MarkSentResponseCount;
    procedure ProcessAllPendingNotifications;
    procedure ReceiveRegister(AOR, Contact: String);
    function  SecondLastEventData: TIdEventData;
    function  ThirdLastEventData: TIdEventData;

    property MockTransport: TIdSipMockTransport read fMockTransport write SetMockTransport;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

//******************************************************************************
//* TIdDataList                                                                *
//******************************************************************************
//* TIdDataList Public methods *************************************************

constructor TIdDataList.Create;
begin
  inherited Create;

  Self.DataList := TObjectList.Create(true);
end;

destructor TIdDataList.Destroy;
begin
  Self.DataList.Free;

  inherited Destroy;
end;

procedure TIdDataList.AddNotification(Data: TIdEventData);
begin
  Self.DataList.Add(Data.Copy);
end;

procedure TIdDataList.CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
var
  Found: Boolean;
  I: Integer;
begin
  Found := false;
  I     := 0;
  while (I < Self.DataList.Count) and not Found do begin
    Found := Self.EventAt(I) is EventType;
    Inc(I);
  end;

  if not Found then Fail(Msg);
end;

procedure TIdDataList.CheckNotificationsReceived(EventType: TIdEventDataClass; ExpectedCount: Cardinal; Msg: String);
var
  ActualCount: Cardinal;
  I:           Integer;
begin
  ActualCount := 0;
  for I := 0 to Self.DataList.Count - 1 do
    if (Self.EventAt(I) is EventType) then
      Inc(ActualCount);

  CheckEquals(ExpectedCount, ActualCount, Msg);
end;

function TIdDataList.EventAt(Index: Integer): TIdEventData;
begin
  Result := Self.DataList[Index] as TIdEventData;
end;

function TIdDataList.LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
var
  Found: Boolean;
  I:     Integer;
begin
  Result := nil;

  Found := false;
  I     := Self.DataList.Count - 1;
  while (I > 0) and not Found do begin
    Found := Self.EventAt(I) is EventType;

    if not Found then Dec(I);
  end;

  if Found then
    Result := Self.EventAt(I)
  else
    Fail('No event of type ' + EventType.ClassName + ' found');
end;

function TIdDataList.SecondLastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 2] as TIdEventData;
end;

function TIdDataList.ThirdLastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 3] as TIdEventData;
end;

//* TIdDataList Private methods ************************************************

procedure TIdDataList.CheckEquals(Expected, Actual: Cardinal; Msg: String);
begin
  if (Expected <> Actual) then
    Fail(Format(Msg + ', expected <%d> but was <%d>', [Expected, Actual]));
end;

procedure TIdDataList.Fail(Msg: String);
begin
  raise ETestFailure.Create(Msg);
end;

//******************************************************************************
//* //* TIdWindowAttachedDataList                                              *
//******************************************************************************
//* TIdWindowAttachedDataList Public methods ***********************************

procedure TIdWindowAttachedDataList.AddNotification(Data: TIdEventData);
begin
  // These Data objects are our responsibility to clean up.
  Self.DataList.Add(Data);
end;

//* TIdWindowAttachedDataList Private methods **********************************

procedure TIdWindowAttachedDataList.OnEvent(Stack: TIdSipStackInterface;
                                            Event: Cardinal;
                                            Data:  TIdEventData);
begin
  Self.AddNotification(Data);
end;

//******************************************************************************
//* TTransportChecking                                                         *
//******************************************************************************
//* TTransportChecking Public methods ******************************************

constructor TTransportChecking.Create;
begin
  inherited Create;

  Self.Acks      := TIdSipRequestList.Create;
  Self.Requests  := TIdSipRequestList.Create;
  Self.Responses := TIdSipResponseList.Create;
end;

destructor TTransportChecking.Destroy;
begin
  Self.Responses.Free;
  Self.Requests.Free;
  Self.Acks.Free;

  inherited Destroy;
end;

procedure TTransportChecking.CheckNoRequestSent(Msg: String);
begin
  Check(Self.RequestCount = Self.Requests.Count, Msg);
end;

procedure TTransportChecking.CheckNoResponseSent(Msg: String);
begin
  Check(Self.ResponseCount = Self.Responses.Count, Msg);
end;

procedure TTransportChecking.CheckRequestSent(Msg: String);
begin
  Check(Self.RequestCount < Self.Requests.Count, Msg);
end;

procedure TTransportChecking.CheckResponseSent(Msg: String);
begin
  Check(Self.ResponseCount < Self.Responses.Count, Msg);
end;

function TTransportChecking.LastSentRequest: TIdSipRequest;
begin
  Result := Self.Requests.Last;
end;

function TTransportChecking.LastSentResponse: TIdSipResponse;
begin
  Result := Self.Responses.Last;
end;

procedure TTransportChecking.MarkSentRequestCount;
begin
  Self.RequestCount := Self.Requests.Count;
end;

procedure TTransportChecking.MarkSentResponseCount;
begin
  Self.ResponseCount := Self.Responses.Count;
end;

function TTransportChecking.SentRequestCount: Integer;
begin
  Result := Self.Requests.Count;
end;

function TTransportChecking.SentResponseCount: Integer;
begin
  Result := Self.Responses.Count;
end;

//* TTransportChecking Private methods *****************************************

procedure TTransportChecking.Check(Condition: Boolean; Msg: String);
begin
  if not Condition then
    raise ETestFailure.Create(Msg);
end;

procedure TTransportChecking.OnException(FailedMessage: TIdSipMessage;
                                         E: Exception;
                                         const Reason: String);
begin
  // Do nothing.
end;

procedure TTransportChecking.OnReceiveRequest(Request: TIdSipRequest;
                                              Receiver: TIdSipTransport;
                                              Source: TIdConnectionBindings);
begin
  if Request.IsAck then
    Self.Acks.AddCopy(Request)
  else
    Self.Requests.AddCopy(Request);
end;

procedure TTransportChecking.OnReceiveResponse(Response: TIdSipResponse;
                                               Receiver: TIdSipTransport;
                                               Source: TIdConnectionBindings);
begin
  Self.Responses.AddCopy(Response);
end;

procedure TTransportChecking.OnRejectedMessage(const Msg: String;
                                               const Reason: String;
                                               Source: TIdConnectionBindings);
begin
  // Do nothing.
end;

procedure TTransportChecking.OnSendRequest(Request: TIdSipRequest;
                                           Sender: TIdSipTransport;
                                           Destination: TIdConnectionBindings);
begin
  if Request.IsAck then
    Self.Acks.AddCopy(Request)
  else
    Self.Requests.AddCopy(Request);
end;

procedure TTransportChecking.OnSendResponse(Response: TIdSipResponse;
                                            Sender: TIdSipTransport;
                                            Destination: TIdConnectionBindings);
begin
  Self.Responses.AddCopy(Response);
end;

//******************************************************************************
//* TStackInterfaceTestCase                                                    *
//******************************************************************************
//* TStackInterfaceTestCase Public methods *************************************

procedure TStackInterfaceTestCase.SetUp;
begin
  inherited SetUp;

  Self.DataList      := TIdWindowAttachedDataList.Create;
  Self.TimerQueue    := TIdDebugTimerQueue.Create(true);
  Self.TransportTest := TTransportChecking.Create;
  Self.UI            := TIdSipStackWindow.CreateNew(nil, Self.DataList);
end;

procedure TStackInterfaceTestCase.TearDown;
begin
  Self.ProcessAllPendingNotifications;
  Self.UI.Release;
  Application.ProcessMessages; // Ensure that Self.UI is gone.
  Self.TransportTest.Free;
  Self.TimerQueue.Terminate;
  Self.DataList.Free;

  inherited TearDown;
end;

//* TStackInterfaceTestCase Protected methods **********************************

procedure TStackInterfaceTestCase.CheckNoRequestSent(Msg: String);
begin
  Self.TransportTest.CheckNoRequestSent(Msg);
end;

procedure TStackInterfaceTestCase.CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
begin
  Self.DataList.CheckNotificationReceived(EventType, Msg);
end;

procedure TStackInterfaceTestCase.CheckNotificationsReceived(EventType: TIdEventDataClass; ExpectedCount: Cardinal; Msg: String);
begin
  Self.DataList.CheckNotificationsReceived(EventType, ExpectedCount, Msg);
end;

procedure TStackInterfaceTestCase.CheckRequestSent(Msg: String);
begin
  Self.TransportTest.CheckRequestSent(Msg);
end;

procedure TStackInterfaceTestCase.CheckResponseSent(Msg: String);
begin
  Self.TransportTest.CheckResponseSent(Msg);
end;

function TStackInterfaceTestCase.EventAt(Index: Integer): TIdEventData;
begin
  Result := Self.DataList.EventAt(Index);
end;

function TStackInterfaceTestCase.LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
begin
  Result := Self.DataList.LastEventOfType(EventType)
end;

function TStackInterfaceTestCase.LastSentRequest: TIdSipRequest;
begin
  Result := Self.TransportTest.LastSentRequest;
end;

function TStackInterfaceTestCase.LastSentResponse: TIdSipResponse;
begin
  Result := Self.TransportTest.LastSentResponse;
end;

procedure TStackInterfaceTestCase.MarkSentRequestCount;
begin
  Self.TransportTest.MarkSentRequestCount;
end;

procedure TStackInterfaceTestCase.MarkSentResponseCount;
begin
  Self.TransportTest.MarkSentResponseCount;
end;

procedure TStackInterfaceTestCase.ProcessAllPendingNotifications;
begin
  Application.ProcessMessages;
end;

procedure TStackInterfaceTestCase.ReceiveRegister(AOR, Contact: String);
var
  From: TIdSipUri;
  Reg:  TIdSipRequest;
begin
  From := TIdSipUri.Create(AOR);
  try
    Reg := TIdSipTestResources.CreateRegister(From, Contact);
    try
      Self.MockTransport.FireOnRequest(Reg);
    finally
      Reg.Free;
    end;
  finally
    From.Free;
  end;
end;

function TStackInterfaceTestCase.SecondLastEventData: TIdEventData;
begin
  Result := Self.DataList.SecondLastEventData
end;

function TStackInterfaceTestCase.ThirdLastEventData: TIdEventData;
begin
  Result := Self.DataList.ThirdLastEventData;
end;

//* TStackInterfaceTestCase Private methods ************************************

procedure TStackInterfaceTestCase.SetMockTransport(Value: TIdSipMockTransport);
begin
  Self.fMockTransport := Value;
  Self.fMockTransport.AddTransportListener(Self.TransportTest);
  Self.fMockTransport.AddTransportSendingListener(Self.TransportTest);
end;

end.
