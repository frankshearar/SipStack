unit Spike;

interface

uses
  audioclasses, Classes, Controls, ExtCtrls, Forms, IdRTP, IdSdp, IdSipCore,
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
    Splitter1: TSplitter;
    Panel2: TPanel;
    Button0: TButton;
    Button9: TButton;
    Button8: TButton;
    Button7: TButton;
    Button6: TButton;
    Button5: TButton;
    Button4: TButton;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    ButtonStar: TButton;
    ButtonHash: TButton;
    ButtonA: TButton;
    ButtonB: TButton;
    ButtonC: TButton;
    ButtonD: TButton;
    ButtonFlash: TButton;
    procedure Button0Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ButtonStarClick(Sender: TObject);
    procedure ButtonHashClick(Sender: TObject);
    procedure ButtonAClick(Sender: TObject);
    procedure ButtonBClick(Sender: TObject);
    procedure ButtonCClick(Sender: TObject);
    procedure ButtonDClick(Sender: TObject);
    procedure ButtonFlashClick(Sender: TObject);
    procedure ByeClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure InviteClick(Sender: TObject);
    procedure UiTimerTimer(Sender: TObject);
  private
    AudioPlayer:  TAudioData;
    CounterLock:  TCriticalSection;
    DataStore:    TStream;
    Dispatch:     TIdSipTransactionDispatcher;
    Lock:         TCriticalSection;
    Media:        TIdSdpPayloadProcessor;
    RTPByteCount: Integer;
    StopEvent:    TEvent;
    TransportUDP: TIdSipTransport;
    UA:           TIdSipUserAgentCore;
    UDPByteCount: Integer;

    procedure Flash(const Btn: TButton);
    procedure LogMessage(const Msg: TIdSipMessage);
    procedure OnChanged(const Observed: TObject);
    procedure OnEstablishedSession(const Session: TIdSipSession);
    procedure OnEndedSession(const Session: TIdSipSession);
    procedure OnModifiedSession(const Session: TIdSipSession;
                                const Invite: TIdSipRequest);
    procedure OnNewData(const Data: TStream;
                        const Port: Integer;
                        const Format: TIdRTPPayload);
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
    procedure ProcessPCMMu(const Data: TStream);
    procedure ProcessTelephoneEvent(const Data: TStream);
    procedure SendDTMF(const Event: Byte);
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
  Dialogs, Graphics, IdGlobal, IdSipConsts, IdSipHeaders, IdSocketHandle,
  IdStack, SysUtils;

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
const
  LocalHostName = '127.0.0.1';
var
  Binding: TIdSocketHandle;
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.StopEvent := TSimpleEvent.Create;

  Self.DataStore := TMemoryStream.Create;
  Self.AudioPlayer := TAudioData.Create;
  Self.AudioPlayer.AutoFreeSource := false;
  Self.AudioPlayer.OnStop := Self.OnPlaybackStopped;
  Self.AudioPlayer.SetFormatParameters(afMuLaw, ChannelsMono, 8000, 8);
  Self.AudioPlayer.Assign(Self.DataStore);
  Self.AudioPlayer.Play(AnyAudioDevice);

  Self.Lock        := TCriticalSection.Create;
  Self.CounterLock := TCriticalSection.Create;

  Self.TransportUDP := TIdSipUdpTransport.Create(IdPORT_SIP);
  if (GStack.LocalAddress <> LocalHostName) then begin
    Binding := Self.TransportUDP.Bindings.Add;
    Binding.IP := GStack.LocalAddress;
    Binding.Port := IdPORT_SIP;
    Self.TransportUDP.HostName := Binding.IP;
  end
  else
    Self.TransportUDP.HostName := LocalHostName;

  Binding := Self.TransportUDP.Bindings.Add;
  Binding.IP := LocalHostName;
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

  Self.CounterLock.Free;
  Self.Lock.Free;

  // If no data at all has arrived we stall here.
  Self.AudioPlayer.Stop;
  Self.StopEvent.WaitFor(10000);
  Self.AudioPlayer.Free;

  Self.DataStore.Free;
  Self.StopEvent.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

procedure TrnidSpike.Flash(const Btn: TButton);
begin
  Btn.Font.Color := clGreen;
  IdGlobal.Sleep(100);
  Btn.Font.Color := clBlack;
end;

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

procedure TrnidSpike.OnNewData(const Data: TStream;
                               const Port: Integer;
                               const Format: TIdRTPPayload);
begin
  if (Lowercase(Format.Name) = Lowercase(PCMMuLawEncoding)) then begin
    Self.ProcessPCMMu(Data);
  end
  else if (Lowercase(Format.Name) = Lowercase(TelephoneEventEncoding)) then begin
    Self.ProcessTelephoneEvent(Data);
  end
  else
    Log.Lines.Add(Format.Name + ' received');
end;

procedure TrnidSpike.OnNewUdpData(const Data: TStream);
begin
  Self.CounterLock.Acquire;
  try
    Inc(Self.UDPByteCount, Data.Size);
  finally
    Self.CounterLock.Release;
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
  Self.StopEvent.SetEvent;
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

procedure TrnidSpike.ProcessPCMMu(const Data: TStream);
begin
  Self.Lock.Acquire;
  try
    Inc(Self.RTPByteCount, Data.Size);

    Self.AudioPlayer.Lock;
    try
      if Assigned(Self.DataStore) then begin
        Self.DataStore.Seek(0, soFromEnd);
        Self.DataStore.CopyFrom(Data, 0);
      end;
    finally
      Self.AudioPlayer.UnLock;
    end;
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.ProcessTelephoneEvent(const Data: TStream);
var
  Tone: Byte;
begin
  Self.Lock.Acquire;
  try
    Data.Read(Tone, SizeOf(Byte));
    case Tone of
      DTMF0:     Self.Flash(Button0);
      DTMF1:     Self.Flash(Button1);
      DTMF2:     Self.Flash(Button2);
      DTMF3:     Self.Flash(Button3);
      DTMF4:     Self.Flash(Button4);
      DTMF5:     Self.Flash(Button5);
      DTMF6:     Self.Flash(Button6);
      DTMF7:     Self.Flash(Button7);
      DTMF8:     Self.Flash(Button8);
      DTMF9:     Self.Flash(Button9);
      DTMFStar:  Self.Flash(ButtonStar);
      DTMFHash:  Self.Flash(ButtonHash);
      DTMFA:     Self.Flash(ButtonA);
      DTMFB:     Self.Flash(ButtonB);
      DTMFC:     Self.Flash(ButtonC);
      DTMFD:     Self.Flash(ButtonD);
      DTMFFlash: Self.Flash(ButtonFlash);
    end;
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.SendDTMF(const Event: Byte);
var
  TE: TIdRTPTelephoneEventPayload;
  S:  TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    TE := Self.Media.Profile.EncodingFor(TelephoneEventEncoding + '/8000').Clone as TIdRTPTelephoneEventPayload;
    try
      TE.Event := Event;
      TE.Duration := 100;
      TE.PrintOn(S);
      // TODO: How do we figure out the real port?
      Self.Media.ServerFor(8002).Session.SendData(TE);
    finally
      TE.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TrnidSpike.StartReadingData(const SDP: String);
begin
  Self.Invite.Enabled := false;
  Self.RTPByteCount   := 0;
  Self.UDPByteCount   := 0;

  Self.Media.StartListening(SDP);
end;

procedure TrnidSpike.StopReadingData;
begin
  Self.Media.StopListening;
  Self.Invite.Enabled := true;
end;

//* TrnidSpike Published methods ***********************************************

procedure TrnidSpike.Button0Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF0);
end;

procedure TrnidSpike.Button1Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF1);
end;

procedure TrnidSpike.Button2Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF2);
end;

procedure TrnidSpike.Button3Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF3);
end;

procedure TrnidSpike.Button4Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF4);
end;

procedure TrnidSpike.Button5Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF5);
end;

procedure TrnidSpike.Button6Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF6);
end;

procedure TrnidSpike.Button7Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF7);
end;

procedure TrnidSpike.Button8Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF8);
end;

procedure TrnidSpike.Button9Click(Sender: TObject);
begin
  Self.SendDTMF(DTMF9);
end;

procedure TrnidSpike.ButtonAClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFA);
end;

procedure TrnidSpike.ButtonBClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFB);
end;

procedure TrnidSpike.ButtonCClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFC);
end;

procedure TrnidSpike.ButtonDClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFD);
end;

procedure TrnidSpike.ButtonStarClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFStar);
end;

procedure TrnidSpike.ButtonHashClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFHash);
end;

procedure TrnidSpike.ButtonFlashClick(Sender: TObject);
begin
  Self.SendDTMF(DTMFFlash);
end;

procedure TrnidSpike.ByeClick(Sender: TObject);
begin
  Self.UA.HangUpAllCalls;
  Self.StopReadingData;
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

procedure TrnidSpike.FormKeyPress(Sender: TObject; var Key: Char);
begin
  //
end;

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

end.
