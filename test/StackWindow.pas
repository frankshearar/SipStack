{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit StackWindow;

interface

uses
  Classes, Contnrs, Forms, IdSipCore, IdSipMessage, IdSipStackInterface,
  IdTimerQueue, IdUdpClient, SyncObjs, SysUtils, TestIdSipStackInterface;

type
  // I provide a message queue to which a StackInterface can send messages. Then
  // I route them back to a test case.
  TStackWindow = class(TForm)
  private
    fTestCase: TestTIdSipStackInterface;

    procedure CMSUCCESS(var Msg: TIdSipEventMessage); message CM_SUCCESS;
    procedure CMFAIL(var Msg: TIdSipEventMessage); message CM_FAIL;
    procedure CMNETWORK_FAILURE(var Msg: TIdSipEventMessage); message CM_NETWORK_FAILURE;
    procedure CMCALL_REQUEST_NOTIFY(var Msg: TIdSipEventMessage); message CM_CALL_REQUEST_NOTIFY;
    procedure CMCALL_ENDED(var Msg: TIdSipEventMessage); message CM_CALL_ENDED;
    procedure CMCALL_ESTABLISHED(var Msg: TIdSipEventMessage); message CM_CALL_ESTABLISHED;
    procedure CMCALL_REMOTE_MODIFY_REQUEST(var Msg: TIdSipEventMessage); message CM_CALL_REMOTE_MODIFY_REQUEST;
    procedure CMCALL_OUTBOUND_MODIFY_SUCCESS(var Msg: TIdSipEventMessage); message CM_CALL_OUTBOUND_MODIFY_SUCCESS;
    procedure CMCALL_PROGRESS(var Msg: TIdSipEventMessage); message CM_CALL_PROGRESS;
    procedure CMCALL_REFERRAL(var Msg: TIdSipEventMessage); message CM_CALL_REFERRAL;
    procedure CMAUTHENTICATION_CHALLENGE(var Msg: TIdSipEventMessage); message CM_AUTHENTICATION_CHALLENGE;
    procedure CMSUBSCRIPTION_ESTABLISHED(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_ESTABLISHED;
    procedure CMSUBSCRIPTION_RECV_NOTIFY(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_RECV_NOTIFY;
    procedure CMSUBSCRIPTION_EXPIRED(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_EXPIRED;
    procedure CMSUBSCRIPTION_REQUEST_NOTIFY(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_REQUEST_NOTIFY;
    procedure CMDEBUG_DROPPED_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_DROPPED_MSG;
    procedure CMDEBUG_RECV_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_RECV_MSG;
    procedure CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_SEND_MSG;
    procedure CMDEBUG_TRANSPORT_EXCEPTION(var Msg: TIdSipEventMessage); message CM_DEBUG_TRANSPORT_EXCEPTION;
    procedure CMDEBUG_TRANSPORT_REJECTED_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_TRANSPORT_REJECTED_MSG;
    procedure CMDEBUG_STACK_STARTED(var Msg: TIdSipEventMessage); message CM_DEBUG_STACK_STARTED;
    procedure CMDEBUG_STACK_STOPPED(var Msg: TIdSipEventMessage); message CM_DEBUG_STACK_STOPPED;

    procedure NotifyTestCase(Msg: TIdSipEventMessage);
  public
    constructor Create(AOwner: TComponent; TestCase: TestTIdSipStackInterface); reintroduce;

    property TestCase: TestTIdSipStackInterface read fTestCase;
  end;

implementation

{$R *.dfm}

//******************************************************************************
//* TStackWindow                                                               *
//******************************************************************************
//* TStackWindow Public methods ************************************************

constructor TStackWindow.Create(AOwner: TComponent; TestCase: TestTIdSipStackInterface);
begin
  inherited Create(AOwner);

  Self.fTestCase := TestCase;
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

procedure TStackWindow.CMCALL_PROGRESS(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMCALL_REFERRAL(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMAUTHENTICATION_CHALLENGE(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMSUBSCRIPTION_ESTABLISHED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMSUBSCRIPTION_RECV_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMSUBSCRIPTION_EXPIRED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMSUBSCRIPTION_REQUEST_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_DROPPED_MSG(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_RECV_MSG(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_TRANSPORT_EXCEPTION(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_TRANSPORT_REJECTED_MSG(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_STACK_STARTED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.CMDEBUG_STACK_STOPPED(var Msg: TIdSipEventMessage);
begin
  Self.NotifyTestCase(Msg);
end;

procedure TStackWindow.NotifyTestCase(Msg: TIdSipEventMessage);
begin
  try
    Self.TestCase.OnEvent(Msg.Data.Stack, Msg.Data.Event, Msg.Data.Data);
  finally
    Msg.Data.Free;
  end;
end;

end.
