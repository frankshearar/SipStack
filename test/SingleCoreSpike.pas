unit SingleCoreSpike;

interface

uses
  ActnList, ActnMan, Controls, Classes, ExtCtrls, Forms, IdSipMessage,
  IdSipStackInterface, StdCtrls;

type
  TSingleCore = class(TForm)
    Panel1: TPanel;
    MessageLog: TMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ToHeader: TEdit;
    Call: TButton;
    HangUp: TButton;
    ActionManager: TActionManager;
    CallAction: TAction;
    HangUpAction: TAction;
    LocalSessionDescription: TMemo;
    Label1: TLabel;
    RemoteSessionDescription: TMemo;
    Label2: TLabel;
    Configuration: TMemo;
    Label3: TLabel;
    RefreshConfig: TButton;
    RefreshConfigAction: TAction;
    Answer: TButton;
    AnswerAction: TAction;
    Transfer: TButton;
    TransferAction: TAction;
    ReferredResource: TEdit;
    FollowRefer: TButton;
    FollowReferAction: TAction;
    ReferredResourceLabel: TLabel;
    procedure CallActionExecute(Sender: TObject);
    procedure HangUpActionExecute(Sender: TObject);
    procedure RefreshConfigClick(Sender: TObject);
    procedure AnswerActionExecute(Sender: TObject);
    procedure TransferActionExecute(Sender: TObject);
    procedure FollowReferActionExecute(Sender: TObject);
  private
    CallHandle:  TIdSipHandle;
    ReferTo:     TIdSipAddressHeader;
    RemoteParty: TIdSipAddressHeader;
    Stack:       TIdSipStackInterface;

    procedure CMAUTHENTICATION_CHALLENGE(var Msg: TIdSipEventMessage); message CM_AUTHENTICATION_CHALLENGE;
    procedure CMSUCCESS(var Msg: TIdSipEventMessage); message CM_SUCCESS;
    procedure CMFAIL(var Msg: TIdSipEventMessage); message CM_FAIL;
    procedure CMNETWORK_FAILURE(var Msg: TIdSipEventMessage); message CM_NETWORK_FAILURE;
    procedure CMCALL_REQUEST_NOTIFY(var Msg: TIdSipEventMessage); message CM_CALL_REQUEST_NOTIFY;
    procedure CMCALL_ENDED(var Msg: TIdSipEventMessage); message CM_CALL_ENDED;
    procedure CMCALL_ESTABLISHED(var Msg: TIdSipEventMessage); message CM_CALL_ESTABLISHED;
    procedure CMCALL_PROGRESS(var Msg: TIdSipEventMessage); message CM_CALL_PROGRESS;
    procedure CMCALL_REFERRAL(var Msg: TIdSipEventMessage); message CM_CALL_REFERRAL;
    procedure CMCALL_REMOTE_MODIFY_REQUEST(var Msg: TIdSipEventMessage); message CM_CALL_REMOTE_MODIFY_REQUEST;
    procedure CMCALL_OUTBOUND_MODIFY_SUCCESS(var Msg: TIdSipEventMessage); message CM_CALL_OUTBOUND_MODIFY_SUCCESS;
    procedure CMSUBSCRIPTION_ESTABLISHED(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_ESTABLISHED;
    procedure CMSUBSCRIPTION_EXPIRED(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_EXPIRED;
    procedure CMSUBSCRIPTION_RECV_NOTIFY(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_RECV_NOTIFY;
    procedure CMSUBSCRIPTION_REQUEST_NOTIFY(var Msg: TIdSipEventMessage); message CM_SUBSCRIPTION_REQUEST_NOTIFY;
    procedure CMDEBUG_DROPPED_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_DROPPED_MSG;
    procedure CMDEBUG_RECV_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_RECV_MSG;
    procedure CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage); message CM_DEBUG_SEND_MSG;
    procedure CMDEBUG_TRANSPORT_EXCEPTION(var Msg: TIdSipEventMessage); message CM_DEBUG_TRANSPORT_EXCEPTION;

    procedure Log(Data: TIdEventData);
    procedure MarkReferral(Data: TIdSubscriptionRequestData);
    procedure NotifyOfCallProgress(Data: TIdSessionProgressData);
    procedure NotifyOfCallReferral(Data: TIdSessionReferralData);
    procedure NotifyOfDroppedMessage(Data: TIdDebugMessageData);
    procedure NotifyOfEndedCall(Data: TIdCallEndedData);
    procedure NotifyOfEstablishedCall(Data: TIdSessionData);
    procedure NotifyOfIncomingCall(Data: TIdSessionData);
    procedure NotifyOfNetworkFailure(Data: TIdFailData);
    procedure NotifyOfReceivedMsg(Data: TIdDebugMessageData);
    procedure NotifyOfReceivedNotify(Data: TIdSubscriptionData);
    procedure NotifyOfRemoteModifyRequest(Data: TIdSessionData);
    procedure NotifyOfSentMsg(Data: TIdDebugSendMessageData);
    procedure NotifyOfSubscriptionEstablished(Data: TIdSubscriptionData);
    procedure NotifyOfSubscriptionExpired(Data: TIdSubscriptionData);
    procedure NotifyOfSubscriptionRequest(Data: TIdSubscriptionRequestData);
    procedure ReceiveNotify(Data: TIdSipStackInterfaceEventMethod);
  public
    constructor CreateUA(AOwner: TComponent;
                         Configuration: TStrings); virtual;
    destructor  Destroy; override;
  end;

var
  SingleCore: TSingleCore;

implementation

{$R *.dfm}

uses
  IdSdp, IdSipSubscribeModule, SysUtils;

//******************************************************************************
//* TSingleCore                                                                *
//******************************************************************************
//* TSingleCore Public methods *************************************************

constructor TSingleCore.CreateUA(AOwner: TComponent;
                                 Configuration: TStrings);
begin
  inherited Create(AOwner);

  Self.Configuration.Text := Configuration.Text;
  Self.ReferTo            := TIdSipReferToHeader.Create;
  Self.RemoteParty        := TIdSipFromHeader.Create;

  Self.Stack      := TIdSipStackInterface.Create(Self.Handle, Configuration);
  Self.CallHandle := InvalidHandle;

  Self.Stack.Resume;
end;

destructor TSingleCore.Destroy;
begin
  Self.Stack.Free;
  Self.ReferTo.Free;
  Self.RemoteParty.Free;

  inherited Destroy;
end;

//* TSingleCore Private methods ************************************************

procedure TSingleCore.CMAUTHENTICATION_CHALLENGE(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMSUCCESS(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMFAIL(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMNETWORK_FAILURE(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_REQUEST_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_ENDED(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_ESTABLISHED(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_PROGRESS(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_REFERRAL(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_REMOTE_MODIFY_REQUEST(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMCALL_OUTBOUND_MODIFY_SUCCESS(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMSUBSCRIPTION_ESTABLISHED(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMSUBSCRIPTION_EXPIRED(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMSUBSCRIPTION_RECV_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMSUBSCRIPTION_REQUEST_NOTIFY(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMDEBUG_DROPPED_MSG(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMDEBUG_RECV_MSG(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMDEBUG_SEND_MSG(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.CMDEBUG_TRANSPORT_EXCEPTION(var Msg: TIdSipEventMessage);
begin
  Self.ReceiveNotify(Msg.Data);
end;

procedure TSingleCore.Log(Data: TIdEventData);
begin
  Self.MessageLog.Lines.Text := Self.MessageLog.Lines.Text + Data.AsString;
end;

procedure TSingleCore.MarkReferral(Data: TIdSubscriptionRequestData);
begin
  if (Data.EventPackage = PackageRefer) then begin
    Self.ReferTo.Assign(Data.ReferTo);
    Self.ReferredResourceLabel.Caption := Data.ReferTo.AsString;
  end;
end;

procedure TSingleCore.NotifyOfCallProgress(Data: TIdSessionProgressData);
begin
end;

procedure TSingleCore.NotifyOfCallReferral(Data: TIdSessionReferralData);
begin
  Self.MarkReferral(Data);
end;

procedure TSingleCore.NotifyOfDroppedMessage(Data: TIdDebugMessageData);
begin
  Self.Log(Data);
end;

procedure TSingleCore.NotifyOfEndedCall(Data: TIdCallEndedData);
begin
  Self.CallHandle := InvalidHandle;
end;

procedure TSingleCore.NotifyOfEstablishedCall(Data: TIdSessionData);
begin
  Self.LocalSessionDescription.Text  := Data.LocalSessionDescription;
  Self.RemoteSessionDescription.Text := Data.RemoteSessionDescription;
end;

procedure TSingleCore.NotifyOfIncomingCall(Data: TIdSessionData);
begin
  Assert(Self.CallHandle = InvalidHandle,
   'You''re already in a call');
  Self.CallHandle := Data.Handle;

  Self.RemoteParty.Assign(Data.RemoteContact);
  Self.RemoteSessionDescription.Text := Data.RemoteSessionDescription;
end;

procedure TSingleCore.NotifyOfNetworkFailure(Data: TIdFailData);
begin
end;

procedure TSingleCore.NotifyOfReceivedMsg(Data: TIdDebugMessageData);
begin
end;

procedure TSingleCore.NotifyOfReceivedNotify(Data: TIdSubscriptionData);
begin
end;

procedure TSingleCore.NotifyOfRemoteModifyRequest(Data: TIdSessionData);
begin
  Self.Stack.AcceptCallModify(Data.Handle,
                              Self.LocalSessionDescription.Text,
                              SdpMimeType);

  Self.RemoteSessionDescription.Text := Data.RemoteSessionDescription;
end;

procedure TSingleCore.NotifyOfSentMsg(Data: TIdDebugSendMessageData);
begin
end;

procedure TSingleCore.NotifyOfSubscriptionEstablished(Data: TIdSubscriptionData);
begin
end;

procedure TSingleCore.NotifyOfSubscriptionExpired(Data: TIdSubscriptionData);
begin
end;

procedure TSingleCore.NotifyOfSubscriptionRequest(Data: TIdSubscriptionRequestData);
begin
  Self.MarkReferral(Data);
end;

procedure TSingleCore.ReceiveNotify(Data: TIdSipStackInterfaceEventMethod);
begin
  try
    Self.Log(Data.Data);

    case Data.Event of
      CM_CALL_ENDED:          Self.NotifyOfEndedCall(Data.Data as TIdCallEndedData);
      CM_CALL_ESTABLISHED:    Self.NotifyOfEstablishedCall(Data.Data as TIdSessionData);
      CM_CALL_PROGRESS:       Self.NotifyOfCallProgress(Data.Data as TIdSessionProgressData);
      CM_CALL_REFERRAL:       Self.NotifyOfCallReferral(Data.Data as TIdSessionReferralData);
      CM_CALL_REMOTE_MODIFY_REQUEST:
                              Self.NotifyOfRemoteModifyRequest(Data.Data as TIdSessionData);
      CM_CALL_REQUEST_NOTIFY: Self.NotifyOfIncomingCall(Data.Data as TIdSessionData);
      CM_NETWORK_FAILURE:     Self.NotifyOfNetworkFailure(Data.Data as TIdFailData);
      CM_SUBSCRIPTION_ESTABLISHED:
                              Self.NotifyOfSubscriptionEstablished(Data.Data as TIdSubscriptionData);
      CM_SUBSCRIPTION_RECV_NOTIFY:
                              Self.NotifyOfReceivedNotify(Data.Data as TIdSubscriptionData);
      CM_SUBSCRIPTION_EXPIRED:Self.NotifyOfSubscriptionExpired(Data.Data as TIdSubscriptionData);
      CM_SUBSCRIPTION_REQUEST_NOTIFY:
                              Self.NotifyOfSubscriptionRequest(Data.Data as TIdSubscriptionRequestData);

      CM_DEBUG_DROPPED_MSG:   Self.NotifyOfDroppedMessage(Data.Data as TIdDebugMessageData);
      CM_DEBUG_RECV_MSG:      Self.NotifyOfReceivedMsg(Data.Data as TIdDebugMessageData);
      CM_DEBUG_SEND_MSG:      Self.NotifyOfSentMsg(Data.Data as TIdDebugSendMessageData);
    end;
  finally
    Data.Free;
  end;
end;

procedure TSingleCore.CallActionExecute(Sender: TObject);
var
  Destination: TIdSipAddressHeader;
begin
  Destination := TIdSipAddressHeader.Create;
  try
    Destination.Value := Self.ToHeader.Text;

    Self.CallHandle := Self.Stack.MakeCall(Destination,
                                           Self.LocalSessionDescription.Text,
                                           SdpMimeType);

    Self.Stack.Send(Self.CallHandle);
  finally
    Destination.Free;
  end;
end;

procedure TSingleCore.HangUpActionExecute(Sender: TObject);
begin
  Self.Stack.HangUp(Self.CallHandle);
end;

procedure TSingleCore.RefreshConfigClick(Sender: TObject);
begin
  Self.Stack.Free;
  Self.Stack := TIdSipStackInterface.Create(Self.Handle,
                                            Self.Configuration.Lines);
end;

procedure TSingleCore.AnswerActionExecute(Sender: TObject);
begin
  Self.Stack.AnswerCall(Self.CallHandle,
                        Self.LocalSessionDescription.Text,
                        SdpMimeType);
end;

procedure TSingleCore.TransferActionExecute(Sender: TObject);
var
  TransferTarget: TIdSipAddressHeader;
  Refer:          TIdSipHandle;
begin
  Assert(Self.CallHandle <> InvalidHandle,
         'You can''t transfer a call when you''re not in a call');

  TransferTarget := TIdSipToHeader.Create;
  try
    TransferTarget.Value := Self.ReferredResource.Text;

    Refer := Self.Stack.MakeRefer(Self.RemoteParty, TransferTarget);
    Self.Stack.Send(Refer);
  finally
    TransferTarget.Free;
  end;
end;

procedure TSingleCore.FollowReferActionExecute(Sender: TObject);
begin
  if Self.ReferTo.Address.IsSipUri then begin
    Self.Stack.HangUp(Self.CallHandle);
    Self.CallHandle := Self.Stack.MakeCall(Self.ReferTo,
                                           Self.LocalSessionDescription.Text,
                                           SdpMimeType);
    Self.Stack.Send(Self.CallHandle);
  end;
end;

end.
