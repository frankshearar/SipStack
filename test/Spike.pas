unit Spike;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSdp, IdSipCore, IdSipMessage,
  IdSipTransaction, IdSipTransport, StdCtrls, SyncObjs;

type
  TrnidSpike = class(TForm,
                     IIdSipDataListener,
                     IIdSipObserver,
                     IIdSipSessionListener,
                     IIdSipTransportListener,
                     IIdSipTransportSendingListener)
    Log: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    SessionCounter: TLabel;
    Label2: TLabel;
    RTPDataCount: TLabel;
    UiTimer: TTimer;
    TargetUri: TEdit;
    Invite: TButton;
    Bye: TButton;
    Label3: TLabel;
    UDPDataCount: TLabel;
    procedure UiTimerTimer(Sender: TObject);
    procedure InviteClick(Sender: TObject);
    procedure ByeClick(Sender: TObject);
  private
    RTPByteCount: Integer;
    UDPByteCount: Integer;
    DataStore:    TStream;
    Dispatch:     TIdSipTransactionDispatcher;
    Lock:         TCriticalSection;
    Media:        TIdSdpPayloadProcessor;
    TransportTCP: TIdSipTransport;
    TransportTLS: TIdSipTransport;
    TransportUDP: TIdSipTransport;
    UA:           TIdSipUserAgentCore;

    procedure LogMessage(const Msg: TIdSipMessage);
    procedure OnChanged(const Observed: TObject);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewData(const Data: TStream);
    procedure OnNewUdpData(const Data: TStream);
    procedure OnNewSession(const Session: TIdSipSession);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpike: TrnidSpike;

implementation

{$R *.dfm}

uses
  Dialogs, IdGlobal, IdSipConsts, IdSipHeaders, IdSocketHandle, IdStack,
  SysUtils;

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
var
  Binding: TIdSocketHandle;
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.RTPByteCount := 0;
  Self.DataStore := TFileStream.Create('..\etc\dump.wav', fmCreate or fmShareDenyWrite);
  Self.Lock      := TCriticalSection.Create;

  Self.TransportUDP := TIdSipUdpTransport.Create(IdPORT_SIP);
  Binding := Self.TransportUDP.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := IdPORT_SIP;
  Self.TransportUDP.HostName := Binding.IP;

  Self.TransportTCP := TIdSipTcpTransport.Create(IdPORT_SIP);
  Binding := Self.TransportTCP.Bindings.Add;
  Binding.IP := Self.TransportUDP.HostName;
  Binding.Port := IdPORT_SIP;
  Self.TransportTCP.HostName := Self.TransportUDP.HostName;

  Self.TransportTLS := TIdSipTLSTransport.Create(IdPORT_SIPS);
  Binding := Self.TransportTLS.Bindings.Add;
  Binding.IP := Self.TransportUDP.HostName;
  Binding.Port := IdPORT_SIPS;
  Self.TransportTLS.HostName := Self.TransportUDP.HostName;

  Self.Media := TIdSdpPayloadProcessor.Create;
  Self.Media.Host := Self.TransportUDP.HostName;
  Self.Media.AddDataListener(Self);

  Self.TransportUDP.AddTransportListener(Self);
  Self.TransportUDP.AddTransportSendingListener(Self);
  Self.TransportTCP.AddTransportListener(Self);
  Self.TransportTCP.AddTransportSendingListener(Self);
  Self.TransportTLS.AddTransportListener(Self);
  Self.TransportTLS.AddTransportSendingListener(Self);
  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.TransportUDP);
  Self.Dispatch.AddTransport(Self.TransportTCP);
  Self.Dispatch.AddTransport(Self.TransportTLS);

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatch;
  Self.UA.AddSessionListener(Self);
  Self.UA.AddObserver(Self);
  Self.UA.HostName := Self.TransportUDP.HostName;
  Self.UA.UserAgentName := 'X-Lite build 1086';

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@' + Self.TransportUDP.HostName;
    Self.UA.Contact := Contact;
  finally
    Contact.Free;
  end;

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'sip:franks@' + Self.TransportUDP.HostName;
    Self.UA.From := From;
  finally
    From.Free;
  end;

  try
    Self.TransportUDP.Start;
    Self.TransportTCP.Start;
    Self.TransportTLS.Start;
  except
    on EIdCouldNotBindSocket do
      ShowMessage('Something''s hogged the SIP port (5060) - '
                + 'kill it and restart this');
  end;
end;

destructor TrnidSpike.Destroy;
begin
  Self.TransportUDP.Stop;

  Self.UA.Free;
  Self.Dispatch.Free;
  Self.Media.Free;
  Self.TransportUDP.Free;
  Self.TransportTLS.Free;
  Self.TransportTCP.Free;
  Self.Lock.Free;
  Self.DataStore.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

procedure TrnidSpike.LogMessage(const Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpike.OnChanged(const Observed: TObject);
begin
  Self.SessionCounter.Caption := IntToStr((Observed as TIdSipUserAgentCore).SessionCount);
end;

procedure TrnidSpike.OnEstablishedSession(const Session: TIdSipSession);
begin
end;

procedure TrnidSpike.OnEndedSession(const Session: TIdSipSession);
begin
  Self.Media.StopListening;
end;

procedure TrnidSpike.OnModifiedSession(const Session: TIdSipSession;
                                       const Invite: TIdSipRequest);
begin
end;

procedure TrnidSpike.OnNewData(const Data: TStream);
begin
  Self.Lock.Acquire;
  try
    Inc(Self.RTPByteCount, Data.Size);

    Self.DataStore.CopyFrom(Data, 0);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnNewUdpData(const Data: TStream);
begin
  Self.Lock.Acquire;
  try
    Inc(Self.UDPByteCount, Data.Size);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnNewSession(const Session: TIdSipSession);
begin
  if (Session.Invite.ContentLength > 0) then
    Self.Media.Process(Session.Invite.Body);

  Session.AcceptCall(Self.Media.LocalSessionDescription, Self.Media.MediaType);
end;

procedure TrnidSpike.OnReceiveRequest(const Request: TIdSipRequest;
                                      const Transport: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpike.OnReceiveResponse(const Response: TIdSipResponse;
                                       const Transport: TIdSipTransport);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpike.OnSendRequest(const Request: TIdSipRequest;
                                   const Transport: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpike.OnSendResponse(const Response: TIdSipResponse;
                                    const Transport: TIdSipTransport);
begin
  Self.LogMessage(Response);
end;

//* TrnidSpike Published methods ***********************************************

procedure TrnidSpike.UiTimerTimer(Sender: TObject);
begin
  Self.Lock.Acquire;
  try
    RTPDataCount.Caption := IntToStr(Self.RTPByteCount);
    UDPDataCount.Caption := IntToStr(Self.UDPByteCount);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.InviteClick(Sender: TObject);
var
  Address: String;
  SDP:     String;
  Target:  TIdSipToHeader;
begin
  Address := Self.TransportUDP.Bindings[0].IP;

  Target := TIdSipToHeader.Create;
  try
    Target.Value := Self.TargetUri.Text;

    SDP := 'v=0'#13#10
         + 'o=franks 123456 123456 IN IP4 ' + Address + #13#10
         + 's=-'#13#10
         + 'c=IN IP4 ' + Address + #13#10
         + 't=0 0'#13#10
         + 'm=audio 8000 RTP/AVP 0'#13#10;

    Self.Media.Process(SDP);

    Self.UA.Call(Target,
                 SDP,
                 SdpMimeType);
  finally
    Target.Free;
  end;
end;

procedure TrnidSpike.ByeClick(Sender: TObject);
begin
  Self.UA.TerminateAllSessions;
  Self.Media.StopListening;
end;

end.
