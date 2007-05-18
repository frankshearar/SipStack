unit TestFrameworkStackInterface;

interface

uses
  Contnrs, Forms, IdSipMessage, IdSipMockTransport, IdSipStackInterface,
  IdTimerQueue, TestFramework, TestFrameworkSip;

type
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
  TStackInterfaceTestCase = class(TTestCase,
                                  IIdSipStackInterface)
  private
    DataList:          TObjectList; // Holds all the data received from the stack
    SentRequestCount:  Cardinal;
    SentResponseCount: Cardinal;
  protected
    MockTransport: TIdSipMockTransport;
    TimerQueue:    TIdDebugTimerQueue;
    UI:            TCustomForm;

    procedure CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
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
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure OnEvent(Stack: TIdSipStackInterface;
                      Event: Cardinal;
                      Data:  TIdEventData);
  end;

implementation

//******************************************************************************
//* TStackInterfaceTestCase                                                    *
//******************************************************************************
//* TStackInterfaceTestCase Public methods *************************************

procedure TStackInterfaceTestCase.SetUp;
begin
  inherited SetUp;

  Self.DataList   := TObjectList.Create(true);
  Self.TimerQueue := TIdDebugTimerQueue.Create(true);
  Self.UI         := TIdSipStackWindow.CreateNew(nil, Self);
end;

procedure TStackInterfaceTestCase.TearDown;
begin
  Self.ProcessAllPendingNotifications;
  Self.UI.Release;
  Self.TimerQueue.Terminate;
  Self.DataList.Free;

  inherited TearDown;
end;

procedure TStackInterfaceTestCase.OnEvent(Stack: TIdSipStackInterface;
                                          Event: Cardinal;
                                          Data:  TIdEventData);
begin
  Self.DataList.Add(Data);
end;

//* TStackInterfaceTestCase Protected methods **********************************

procedure TStackInterfaceTestCase.CheckNotificationReceived(EventType: TIdEventDataClass; Msg: String);
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

procedure TStackInterfaceTestCase.CheckRequestSent(Msg: String);
begin
  Check(Self.SentRequestCount < Self.MockTransport.SentRequestCount, Msg);
end;

procedure TStackInterfaceTestCase.CheckResponseSent(Msg: String);
begin
  Check(Self.SentResponseCount < Self.MockTransport.SentResponseCount, Msg);
end;

function TStackInterfaceTestCase.EventAt(Index: Integer): TIdEventData;
begin
  Result := Self.DataList[Index] as TIdEventData;
end;

function TStackInterfaceTestCase.LastEventOfType(EventType: TIdEventDataClass): TIdEventData;
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

function TStackInterfaceTestCase.LastSentRequest: TIdSipRequest;
begin
  Result := Self.MockTransport.LastRequest;
end;

function TStackInterfaceTestCase.LastSentResponse: TIdSipResponse;
begin
  Result := Self.MockTransport.LastResponse;
end;

procedure TStackInterfaceTestCase.MarkSentRequestCount;
begin
  Self.SentRequestCount := Self.MockTransport.SentRequestCount;
end;

procedure TStackInterfaceTestCase.MarkSentResponseCount;
begin
  Self.SentResponseCount := Self.MockTransport.SentResponseCount;
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
  Result := Self.DataList[Self.DataList.Count - 2] as TIdEventData;
end;

function TStackInterfaceTestCase.ThirdLastEventData: TIdEventData;
begin
  Result := Self.DataList[Self.DataList.Count - 3] as TIdEventData;
end;

end.
