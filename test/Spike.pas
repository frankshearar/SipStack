unit Spike;

interface

uses
  audioclasses, Classes, Controls, ExtCtrls, Forms, IdSdp, IdSipCore,
  IdSipMessage, IdSipTransaction, IdSipTransport, StdCtrls, SyncObjs;

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
    AudioPlayer:  TAudioData;
    DataStore:    TStream;
    Dispatch:     TIdSipTransactionDispatcher;
    Lock:         TCriticalSection;
    Media:        TIdSdpPayloadProcessor;
    RTPByteCount: Integer;
    TransportUDP: TIdSipTransport;
    UA:           TIdSipUserAgentCore;
    UDPByteCount: Integer;

    procedure LogMessage(const Msg: TIdSipMessage);
    procedure OnChanged(const Observed: TObject);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewData(const Data: TStream);
    procedure OnNewUdpData(const Data: TStream);
    procedure OnNewSession(const Session: TIdSipSession);
    procedure OnPlaybackStopped(Origin: TAudioData);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
    procedure StartPlayback;
    procedure StartReadingData(const SDP: String);
    procedure StopReadingData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

const
  AudioFile = 'dump.wav';

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

  Self.Lock := TCriticalSection.Create;

  Self.TransportUDP := TIdSipUdpTransport.Create(IdPORT_SIP);
  Binding := Self.TransportUDP.Bindings.Add;
  Binding.IP := GStack.LocalAddress;
  Binding.Port := IdPORT_SIP;
  Self.TransportUDP.HostName := Binding.IP;
  Binding := Self.TransportUDP.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := IdPORT_SIP;

  Self.Media := TIdSdpPayloadProcessor.Create;
  Self.Media.Host := Self.TransportUDP.HostName;
  Self.Media.AddDataListener(Self);

  Self.TransportUDP.AddTransportListener(Self);
  Self.TransportUDP.AddTransportSendingListener(Self);
  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.TransportUDP);

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

  Self.Lock.Free;
  Self.DataStore.Free;
  Self.AudioPlayer.Free;

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
  Self.StopReadingData;
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

    if Assigned(Self.DataStore) then begin
      Self.DataStore.Seek(0, soFromEnd);
      Self.DataStore.CopyFrom(Data, 0);
    end;
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
    Self.StartReadingData(Session.Invite.Body);

  Session.AddSessionListener(Self);
  Session.AcceptCall(Self.Media.LocalSessionDescription, Self.Media.MediaType);
end;

procedure TrnidSpike.OnPlaybackStopped(Origin: TAudioData);
begin
  Self.AudioPlayer.Stop;
  FreeAndNil(Self.AudioPlayer);
  Self.Invite.Enabled := true;
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

procedure TrnidSpike.StartPlayback;
begin
  Self.AudioPlayer := TAudioData.Create;
  Self.AudioPlayer.OnStop := Self.OnPlaybackStopped;
  Self.AudioPlayer.SetFormatParameters(afMuLaw, ChannelsMono, 8000, 8);
  Self.AudioPlayer.LoadFromFile(AudioFile);
  Self.AudioPlayer.Play(AnyAudioDevice);
end;

procedure TrnidSpike.StartReadingData(const SDP: String);
begin
  if FileExists(AudioFile) then
    DeleteFile(AudioFile);
    FileClose(FileCreate(AudioFile));

  if not Assigned(Self.DataStore) then
    Self.DataStore := TFileStream.Create(AudioFile, fmOpenWrite);

  Self.Invite.Enabled := false;
  Self.RTPByteCount   := 0;
  Self.UDPByteCount   := 0;

  Self.Media.StartListening(SDP);
end;

procedure TrnidSpike.StopReadingData;
begin
  Self.Media.StopListening;

  FreeAndNil(Self.DataStore);
  Sleep(500);
  Self.StartPlayback;
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

    Self.StartReadingData(SDP);

    Self.UA.Call(Target,
                 SDP,
                 SdpMimeType).AddSessionListener(Self);
  finally
    Target.Free;
  end;
end;

procedure TrnidSpike.ByeClick(Sender: TObject);
begin
  Self.UA.HangUpAllCalls;
  Self.StopReadingData;
end;

end.
