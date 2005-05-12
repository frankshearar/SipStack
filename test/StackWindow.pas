unit StackWindow;

interface

uses
  Classes, Contnrs, Forms, IdSipCore, IdSipMessage, IdSipStackInterface,
  IdTimerQueue, IdUdpClient, SyncObjs, SysUtils, TestIdSipStackInterface;

type
  TStackWindow = class(TForm)
  private
    DefaultTimeout: Cardinal;
    EmptyListEvent: TEvent;
    ExceptionType:  ExceptClass;
    fTestCase:      TestTIdSipStackInterface;
    Intf:           TIdSipStackInterface;

    procedure CMSUCCESS(var Msg: TIdSipEventMessage); message CM_SUCCESS;
    procedure CMFAIL(var Msg: TIdSipEventMessage); message CM_FAIL;
    procedure CMNETWORK_FAILURE(var Msg: TIdSipEventMessage); message CM_NETWORK_FAILURE;
    procedure CMCALL_REQUEST_NOTIFY(var Msg: TIdSipEventMessage); message CM_CALL_REQUEST_NOTIFY;
    procedure CMCALL_ENDED(var Msg: TIdSipEventMessage); message CM_CALL_ENDED;
    procedure CMCALL_ESTABLISHED(var Msg: TIdSipEventMessage); message CM_CALL_ESTABLISHED;
    procedure CMCALL_REMOTE_MODIFY_REQUEST(var Msg: TIdSipEventMessage); message CM_CALL_REMOTE_MODIFY_REQUEST;
    procedure CMCALL_OUTBOUND_MODIFY_SUCCESS(var Msg: TIdSipEventMessage); message CM_CALL_OUTBOUND_MODIFY_SUCCESS;
    procedure CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_SEND_MSG;

    procedure NotifyTestCase(Msg: TIdSipEventMessage);
    procedure OnStackQueueEmpty(Sender: TIdTimerQueue);
    procedure WaitForSignaled(Event: TEvent; Msg: String);
  public
    constructor Create(AOwner: TComponent; TestCase: TestTIdSipStackInterface); reintroduce;
    destructor  Destroy; override;

    property TestCase: TestTIdSipStackInterface read fTestCase;
  end;

implementation

uses
  IdSipTransport;

{$R *.dfm}

//******************************************************************************
//* TStackWindow                                                               *
//******************************************************************************
//* TStackWindow Public methods ************************************************

constructor TStackWindow.Create(AOwner: TComponent; TestCase: TestTIdSipStackInterface);
var
  BasicConf: TStrings;
begin
  inherited Create(AOwner);

  Self.DefaultTimeout := 1000;
  Self.EmptyListEvent := TSimpleEvent.Create;
  Self.ExceptionType  := Exception;
  Self.fTestCase      := TestCase;

  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipUDPTransport);

  BasicConf := TStringList.Create;
  try
    BasicConf.Add('Listen: UDP AUTO:5060');
    BasicConf.Add('NameServer: 62.241.160.200:53');
    BasicConf.Add('Contact: sip:foo@' + LocalAddress + ':5060');
    BasicConf.Add('From: sip:foo@' + LocalAddress + ':5060');

    Self.Intf := TIdSipStackInterface.Create(Self.Handle, BasicConf);
  finally
    BasicConf.Free;
  end;
  Self.Intf.OnEmpty := Self.OnStackQueueEmpty;
  Self.Intf.Resume;


  Self.TestCase.Intf := Self.Intf;
end;

destructor TStackWindow.Destroy;
begin
  Self.Intf.Terminate;
  Self.WaitForSignaled(Self.EmptyListEvent,
                       'Stack took too long to finish handling outstanding events');

  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);

  Self.EmptyListEvent.Free;

  inherited Destroy;
end;

//* TStackWindow Private methods ***********************************************

procedure TStackWindow.CMSUCCESS(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMFAIL(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMNETWORK_FAILURE(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_REQUEST_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_ENDED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_ESTABLISHED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_REMOTE_MODIFY_REQUEST(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_OUTBOUND_MODIFY_SUCCESS(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.NotifyTestCase(Msg: TIdSipEventMessage);
begin
  Self.TestCase.OnEvent(Msg.Data.Stack, Msg.Data.Event, Msg.Data.Data);

  Msg.Data.Free;
end;

procedure TStackWindow.OnStackQueueEmpty(Sender: TIdTimerQueue);
begin
  Self.EmptyListEvent.SetEvent;
end;

procedure TStackWindow.WaitForSignaled(Event: TEvent; Msg: String);
begin
  if (wrSignaled <> Event.WaitFor(Self.DefaultTimeout)) then
    raise Self.ExceptionType.Create(Msg);

  Event.ResetEvent;
end;

end.
