{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}

{ CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING

  This is a PROTOTYPE. You WILL find all sorts of bugs and things in things like
  buttons not en/disabling etc. The unit does provide you with some examples of
  how to use the SIP stack, and a fairly decent testing User Agent for fiddling
  with the call control SIP provides.

  CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING CAVEAT WARNING
}
unit Spike;

interface

uses
  audioclasses, Classes, Contnrs, Controls, ExtCtrls, Forms, IdDTMFPanel,
  IdObservable, IdRTPDiagnostics, IdRTP, IdSdp, IdSipCore, IdSipIndyLocator,
  IdSipMessage, IdSipTransaction, IdSipTransport, IdSocketHandle, IdTimerQueue,
  StdCtrls, SyncObjs, SysUtils;

type
  TrnidSpike = class(TForm,
                     IIdRTPDataListener,
                     IIdRTPListener,
                     IIdObserver,
                     IIdSipActionListener,
                     IIdSipOptionsListener,
                     IIdSipRegistrationListener,
                     IIdSipSessionListener,
                     IIdSipTransportListener,
                     IIdSipTransportSendingListener,
                     IIdSipUserAgentListener)
    UiTimer: TTimer;
    Splitter1: TSplitter;
    IOPanel: TPanel;
    DebugPanel: TPanel;
    Log: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    SessionCounter: TLabel;
    Label2: TLabel;
    RTPDataCount: TLabel;
    Label3: TLabel;
    UDPDataCount: TLabel;
    TargetUri: TEdit;
    Invite: TButton;
    Bye: TButton;
    InputSplitter: TSplitter;
    UpperInput: TPanel;
    Splitter3: TSplitter;
    InputText: TMemo;
    TextTimer: TTimer;
    BasePort: TEdit;
    Password: TEdit;
    LowerInput: TPanel;
    Splitter4: TSplitter;
    OutputText: TMemo;
    Unregister: TButton;
    Register: TButton;
    Options: TButton;
    UseAsProxy: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    UseLooseRouting: TCheckBox;
    ContactUri: TEdit;
    HostName: TEdit;
    Label6: TLabel;
    MasqAsNat: TCheckBox;
    Answer: TButton;
    Edit1: TEdit;
    Label7: TLabel;
    FromUri: TEdit;
    Label8: TLabel;
    procedure ByeClick(Sender: TObject);
    procedure InviteClick(Sender: TObject);
    procedure UiTimerTimer(Sender: TObject);
    procedure TextTimerTimer(Sender: TObject);
    procedure InputTextKeyPress(Sender: TObject; var Key: Char);
    procedure BasePortChange(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UnregisterClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure ContactUriChange(Sender: TObject);
    procedure UseAsProxyClick(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure HostNameChange(Sender: TObject);
    procedure AnswerClick(Sender: TObject);
    procedure FromUriChange(Sender: TObject);
  private
    CounterLock:    TCriticalSection;
    Lock:           TCriticalSection;
    TextLock:       TCriticalSection;

    AudioPlayer:    TAudioData;
    DataStore:      TStream;
    DTMFPanel:      TIdDTMFPanel;
    HistListener:   TIdRTPPayloadHistogram;
    HistogramPanel: TIdHistogramPanel;
    LatestSession:  TIdSipSession;
    RTPByteCount:   Integer;
    RTPProfile:     TIdRTPProfile;
    RunningPort:    Cardinal;
    SendBuffer:     String;
    StopEvent:      TEvent;
    Timer:          TIdTimerQueue;
    UA:             TIdSipUserAgent;
    UDPByteCount:   Integer;

    fPayloadProcessor:    TIdSDPMultimediaSession;

    function  Address: String;
    procedure CreateUi;
    function  LocalSDP(const Address: String): String;
    procedure LogMessage(Msg: TIdSipMessage; Inbound: Boolean);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse); overload;
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean); overload;
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure OnEstablishedSession(Session: TIdSipSession;
                                   const RemoteSessionDescription: String;
                                   const MimeType: String);
    procedure OnEndedSession(Session: TIdSipSession;
                             ErrorCode: Cardinal);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                            Session: TIdSipInboundSession);
    procedure OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                    Subscription: TIdSipInboundSubscribe);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Answer: TIdSipResponse);
    procedure OnModifySession(Session: TIdSipSession;
                              const RemoteSessionDescription: String;
                              const MimeType: String);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnection);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnection);
    procedure OnPlaybackStopped(Origin: TAudioData);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure ProcessPCM(Data: TStream);
    procedure ProcessText(Text: String);
    procedure ResetCounters;
    procedure StartTransports;
    procedure StopReadingData;
    procedure StopTransports;
    procedure UpdateCounters;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property PayloadProcessor: TIdSDPMultimediaSession read fPayloadProcessor;
  end;

var
  rnidSpike: TrnidSpike;

implementation

{$R *.dfm}

uses
  Dialogs, Graphics, IdGlobal, IdSimpleParser, IdSipConsts, IdStack;

const
  LocalHostName = '127.0.0.1';

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
var
  Conf: TStrings;
  Configurator: TIdSipStackConfigurator;
  I: Integer;
begin
  inherited Create(AOwner);

  Self.RunningPort := 35060;
  Self.CreateUi;

  TIdSipTransportRegistry.RegisterTransport(TcpTransport, TIdSipTcpTransport);
  TIdSipTransportRegistry.RegisterTransport(TlsTransport, TIdSipTlsTransport);
  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipUdpTransport);

  Self.Timer := TIdThreadedTimerQueue.Create(false);

  Conf := TStringList.Create;
  try
    Conf.Add('Listen: UDP AUTO:' + IntToStr(Self.RunningPort));
    Conf.Add('Listen: TCP AUTO:' + IntToStr(Self.RunningPort));
    Conf.Add('Contact: ' + Self.ContactUri.Text);
    Conf.Add('From: ' + Self.FromUri.Text);
    Conf.Add('NameServer: MOCK');

    if Self.UseAsProxy.Checked then
      Conf.Add('Proxy: ' + Self.TargetUri.Text + ';lr');

    Configurator := TIdSipStackConfigurator.Create;
    try
      Self.UA := Configurator.CreateUserAgent(Conf, Self.Timer);
    finally
      Configurator.Free;
    end;
  finally
    Conf.Free;
  end;

  Self.Timer.Resume;

  Self.RTPProfile := TIdAudioVisualProfile.Create;

  Self.UA.AddUserAgentListener(Self);
  Self.UA.AddObserver(Self);
  Self.UA.AddModule(TIdSipRegisterModule);

  for I := 0 to Self.UA.Dispatcher.Transports.Count - 1 do begin
    Self.UA.Dispatcher.Transports[I].AddTransportListener(Self);
    Self.UA.Dispatcher.Transports[I].AddTransportSendingListener(Self);
  end;

  try
    Self.StopTransports;
    BasePort.Text := IntToStr(Self.UA.Dispatcher.Transports[0].Port);
    Self.StartTransports;
  except
    on EIdCouldNotBindSocket do
      ShowMessage('Something''s hogged the SIP port '
                + '(' + IntToStr(Self.UA.Dispatcher.Transports[0].Port) + ') - '
                + 'kill it and restart this');
  end;
  Self.fPayloadProcessor := TIdSDPMultimediaSession.Create(Self.RTPProfile);
end;

destructor TrnidSpike.Destroy;
begin
  Self.Timer.Terminate;
  Self.StopTransports;

  Self.PayloadProcessor.Free;

  Self.UA.Free;

  Self.TextLock.Free;
  Self.CounterLock.Free;
  Self.Lock.Free;

  // If no data at all has arrived we stall here.
  Self.AudioPlayer.Stop;
  Self.StopEvent.WaitFor(1000);
  Self.AudioPlayer.Free;

  Self.DataStore.Free;
  Self.StopEvent.Free;

  Self.HistListener.Free;
  Self.HistogramPanel.Free;
  Self.DTMFPanel.Free;
  Self.RTPProfile.Free;

  TIdSipTransportRegistry.UnregisterTransport(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransport(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

function TrnidSpike.Address: String;
begin
  Result := Self.UA.Dispatcher.Transports[0].Address;
end;

procedure TrnidSpike.CreateUi;
begin
  Self.DTMFPanel := TIdDTMFPanel.Create(nil);
  Self.DTMFPanel.Align  := alLeft;
  Self.DTMFPanel.Left   := -1; // Forces the panel left of the splitter
  Self.DTMFPanel.Parent := Self.UpperInput;
  Self.DTMFPanel.Top    := 0;

  Self.HistogramPanel := TIdHistogramPanel.Create(nil);
  Self.HistogramPanel.Align      := alLeft;
  Self.HistogramPanel.BevelInner := bvNone;
  Self.HistogramPanel.BevelOuter := bvNone;
  Self.HistogramPanel.Left       := -1; // Forces the panel left of the splitter
  Self.HistogramPanel.Parent     := Self.LowerInput;
  Self.HistogramPanel.Top        := 0;
  Self.HistogramPanel.Width      := Self.DTMFPanel.Width;

  Self.HistListener := TIdRTPPayloadHistogram.Create;
  Self.HistListener.AddObserver(Self.HistogramPanel);

  Self.Lock        := TCriticalSection.Create;
  Self.CounterLock := TCriticalSection.Create;
  Self.TextLock    := TCriticalSection.Create;

  Self.DataStore := TMemoryStream.Create;
  Self.AudioPlayer := TAudioData.Create;
  Self.AudioPlayer.AutoFreeSource := false;
  Self.AudioPlayer.OnStop := Self.OnPlaybackStopped;
  Self.AudioPlayer.SetFormatParameters(afMuLaw, ChannelsMono, 8000, 8);
  Self.AudioPlayer.Assign(Self.DataStore);

  Self.StopEvent := TSimpleEvent.Create;
end;

function TrnidSpike.LocalSDP(const Address: String): String;
begin
  Result := 'v=0'#13#10
          + 'o=franks 123456 123456 IN IP4 ' + Address + #13#10
          + 's=-'#13#10
          + 'c=IN IP4 ' + Address + #13#10
          + 't=0 0'#13#10
//          + 'm=audio 8000 RTP/AVP 0'#13#10
//          + 'm=audio 8002 RTP/AVP 96'#13#10
//          + 'a=rtpmap:96 telephone-event/8000'#13#10
//          + 'a=fmtp:101 0-16'#13#10
          + 'm=text 8004 RTP/AVP 97'#13#10
          + 'a=rtpmap:97 T140/1000'#13#10;
end;

procedure TrnidSpike.LogMessage(Msg: TIdSipMessage; Inbound: Boolean);
var
  LogLine: String;
begin
  if Inbound then begin
    LogLine := '<<<< ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now);
  end
  else begin
    LogLine := '>>>> ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now);
  end;

  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add(LogLine);
    Self.Log.Lines.Add(EncodeNonLineUnprintableChars(Msg.AsString));
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnAuthenticationChallenge(Action: TIdSipAction;
                                               Response: TIdSipResponse);
begin
  raise Exception.Create('TrnidSpike.OnAuthenticationChallenge');
end;

procedure TrnidSpike.OnAuthenticationChallenge(UserAgent: TIdSipAbstractUserAgent;
                                               Challenge: TIdSipResponse;
                                               var Username: String;
                                               var Password: String;
                                               var TryAgain: Boolean);
begin
  Username := UserAgent.Contact.Address.Username;
  Password := Self.Password.Text;
  TryAgain := true;
end;

procedure TrnidSpike.OnChanged(Observed: TObject);
begin
  Self.SessionCounter.Caption := IntToStr((Observed as TIdSipUserAgent).SessionCount);
end;

procedure TrnidSpike.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractUserAgent;
                                               Message: TIdSipMessage;
                                               Receiver: TIdSipTransport);
const
  LogLine = 'Dropped unmatched message: %s';
begin
  Self.Lock.Acquire;
  try
    if Message.IsRequest then
      Self.Log.Lines.Add(Format(LogLine, [(Message as TIdSipRequest).Method]))
    else
      Self.Log.Lines.Add(Format(LogLine, [(Message as TIdSipResponse).Description]));
  finally
    Self.Lock.Release;
  end;
  Self.LogMessage(Message, true);
end;

procedure TrnidSpike.OnEstablishedSession(Session: TIdSipSession;
                                          const RemoteSessionDescription: String;
                                          const MimeType: String);
begin
  Self.PayloadProcessor.SetRemoteDescription(RemoteSessionDescription);
end;

procedure TrnidSpike.OnEndedSession(Session: TIdSipSession;
                                    ErrorCode: Cardinal);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('Session ended: ' + IntToStr(ErrorCode));
  finally
    Self.Lock.Release;
  end;

  Self.LatestSession := nil;

  Self.AudioPlayer.Stop;
  Self.StopReadingData;
  Self.Answer.Enabled := false;
end;

procedure TrnidSpike.OnException(E: Exception;
                                 const Reason: String);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('Exception ' + E.ClassName + ': ' + E.Message
                     + ' raised because: ''' + Reason + '''')
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnNetworkFailure(Action: TIdSipAction;
                                      ErrorCode: Cardinal;
                                      const Reason: String);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('|||| ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now));

    Self.Log.Lines.Add(IntToStr(ErrorCode) + ': ' + Reason);
  finally
    Self.Lock.Release;
  end;

  if (Action = Self.LatestSession) then begin
    Self.Answer.Enabled := false;
    Self.StopReadingData;
  end;
end;

procedure TrnidSpike.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                               CurrentBindings: TIdSipContacts;
                               const Reason: String);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('|||| ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now));

    Self.Log.Lines.Add(Reason);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnInboundCall(UserAgent: TIdSipAbstractUserAgent;
                                   Session: TIdSipInboundSession);
begin
  Session.AddSessionListener(Self);
  Self.LatestSession := Session;
  Self.Answer.Enabled := true;
  Self.Answer.Click;
end;

procedure TrnidSpike.OnSubscriptionRequest(UserAgent: TIdSipAbstractUserAgent;
                                           Subscription: TIdSipInboundSubscribe);
begin
end;                                           

procedure TrnidSpike.OnModifiedSession(Session: TIdSipSession;
                                       Answer: TIdSipResponse);
begin
end;

procedure TrnidSpike.OnModifySession(Session: TIdSipSession;
                                     const RemoteSessionDescription: String;
                                     const MimeType: String);
begin
  Session.AcceptModify(Self.LocalSDP(Self.UA.Dispatcher.Transports[0].Address),
                       SdpMimeType);
end;

procedure TrnidSpike.OnNewData(Data: TIdRTPPayload;
                               Binding: TIdConnection);
var
  S: TStringStream;
begin
  if IsEqual(Data.Name, PCMMuLawEncoding) then begin
    S := TStringStream.Create((Data as TIdRTPRawPayload).Data);
    try
      Self.ProcessPCM(S);
    finally
      S.Free;
    end;
  end
  else if (Data is TIdRTPT140Payload) then begin
    Self.ProcessText((Data as TIdRTPT140Payload).Block);
  end;
end;

procedure TrnidSpike.OnRTCP(Packet: TIdRTCPPacket;
                            Binding: TIdConnection);
begin
end;

procedure TrnidSpike.OnRTP(Packet: TIdRTPPacket;
                           Binding: TIdConnection);
begin
  Self.CounterLock.Acquire;
  try
    Inc(Self.RTPByteCount, Packet.Payload.Length);
  finally
    Self.CounterLock.Release;
  end;
end;

procedure TrnidSpike.OnPlaybackStopped(Origin: TAudioData);
begin
  Self.StopEvent.SetEvent;
end;

procedure TrnidSpike.OnReceiveRequest(Request: TIdSipRequest;
                                      Receiver: TIdSipTransport);
begin
  Self.LogMessage(Request, true);
end;

procedure TrnidSpike.OnReceiveResponse(Response: TIdSipResponse;
                                       Receiver: TIdSipTransport);
begin
  Self.LogMessage(Response, true);
end;

procedure TrnidSpike.OnRejectedMessage(const Msg: String;
                                       const Reason: String);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('----REJECTED MESSAGE: ' + Reason + '----');
    Self.Log.Lines.Add(Msg);
    Self.Log.Lines.Add('----');
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                Response: TIdSipResponse);
begin
end;

procedure TrnidSpike.OnSendRequest(Request: TIdSipRequest;
                                   Sender: TIdSipTransport);
begin
  // Don't ever do this: we're on a private LAN accessing the SIP network
  // through a NATting firewall. Doing the below makes us look like the
  // firewall itself to things in the Internet. If you want to have a NATting
  // firewall, use a SIP proxy on the firewall.

  if Self.MasqAsNat.Checked then
    Request.LastHop.SentBy := Self.HostName.Text;

  Self.LogMessage(Request, false);
end;

procedure TrnidSpike.OnSendResponse(Response: TIdSipResponse;
                                    Sender: TIdSipTransport);
begin
  Self.LogMessage(Response, false);
end;

procedure TrnidSpike.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                               CurrentBindings: TIdSipContacts);
begin
end;

procedure TrnidSpike.ProcessPCM(Data: TStream);
begin
  Self.AudioPlayer.Lock;
  try
    if Assigned(Self.DataStore) then begin
      Self.DataStore.Seek(0, soFromEnd);
      Self.DataStore.CopyFrom(Data, 0);
    end;
  finally
    Self.AudioPlayer.UnLock;
  end;
end;

procedure TrnidSpike.ProcessText(Text: String);
begin
  Self.TextLock.Acquire;
  try
    Self.OutputText.Text := Self.OutputText.Text + Text;
  finally
    Self.TextLock.Release;
 end;
end;

procedure TrnidSpike.ResetCounters;
begin
  Self.CounterLock.Acquire;
  try
    Self.Invite.Enabled := false;
    Self.RTPByteCount   := 0;
    Self.UDPByteCount   := 0;
  finally
    Self.CounterLock.Release;
  end;
end;

procedure TrnidSpike.StartTransports;
var
  I: Integer;
begin
  for I := 0 to Self.UA.Dispatcher.Transports.Count - 1 do
    Self.UA.Dispatcher.Transports[I].Start;
end;

procedure TrnidSpike.StopReadingData;
begin
  Self.Invite.Enabled := true;
end;

procedure TrnidSpike.StopTransports;
var
  I: Integer;
begin
  for I := 0 to Self.UA.Dispatcher.Transports.Count - 1 do
    Self.UA.Dispatcher.Transports[I].Stop;
end;

procedure TrnidSpike.UpdateCounters;
begin
  Self.CounterLock.Acquire;
  try
    RTPDataCount.Caption := IntToStr(Self.RTPByteCount);
    UDPDataCount.Caption := IntToStr(Self.UDPByteCount);
  finally
    Self.CounterLock.Release;
  end;
end;

//* TrnidSpike Published methods ***********************************************

procedure TrnidSpike.ByeClick(Sender: TObject);
begin
  if Assigned(Self.LatestSession) then
    Self.LatestSession.Terminate;

  Self.StopReadingData;
end;

procedure TrnidSpike.InviteClick(Sender: TObject);
var
  I:           Integer;
  OurHostName: String;
  SDP:         String;
  Target:      TIdSipToHeader;
begin
  OurHostName := Self.UA.Dispatcher.Transports[0].Address;

  SDP := Self.LocalSDP(OurHostName);

  Target := TIdSipToHeader.Create;
  try
    Target.Address.Uri := Self.TargetUri.Text;

    Self.ResetCounters;

//    Self.AudioPlayer.Play(AnyAudioDevice);
    SDP := Self.PayloadProcessor.StartListening(SDP);
    for I := 0 to Self.PayloadProcessor.StreamCount - 1 do
      Self.PayloadProcessor.Streams[I].AddDataListener(Self);

    if Self.MasqAsNat.Checked then
      SDP := StringReplace(SDP, OurHostName, Self.HostName.Text, [rfReplaceAll, rfIgnoreCase]);

    Self.LatestSession := Self.UA.Call(Target,
                            SDP,
                            SdpMimeType);
    Self.LatestSession.AddSessionListener(Self);                        
    Self.LatestSession.Send;
  finally
    Target.Free;
  end;
end;

procedure TrnidSpike.UiTimerTimer(Sender: TObject);
begin
  Self.UpdateCounters;
end;

procedure TrnidSpike.TextTimerTimer(Sender: TObject);
var
  Text: TIdRTPT140Payload;
begin
  Self.TextLock.Acquire;
  try
    if (Self.SendBuffer <> '') then begin
      Text := Self.RTPProfile.EncodingFor(T140Encoding).Clone as TIdRTPT140Payload;
      try
        Text.Block := Self.SendBuffer;
        Self.PayloadProcessor.Streams[0].SendData(Text);
      finally
        Text.Free;
      end;
      Self.SendBuffer := '';
    end;
  finally
    Self.TextLock.Release;
  end;
end;

procedure TrnidSpike.InputTextKeyPress(Sender: TObject; var Key: Char);
begin
  Self.TextLock.Acquire;
  try
    Self.SendBuffer := Self.SendBuffer + Key;
  finally
    Self.TextLock.Release;
  end;
end;

procedure TrnidSpike.BasePortChange(Sender: TObject);
var
  I:       Integer;
  NewPort: Integer;
begin
  Self.StopTransports;

  NewPort := StrToInt(BasePort.Text);

  for I := 0 to Self.UA.Dispatcher.Transports.Count - 1 do
    Self.UA.Dispatcher.Transports[I].Port := NewPort;

  Self.UA.Contact.Address.Port := NewPort;
  Self.UA.From.Address.Port    := NewPort;

  Self.StartTransports;
end;

procedure TrnidSpike.RegisterClick(Sender: TObject);
var
  Reg:       TIdSipOutboundRegister;
  Registrar: TIdSipUri;
begin
  Registrar := TIdSipUri.Create(Self.TargetUri.Text);
  try
    Reg := Self.UA.RegisterWith(Registrar);
    Reg.AddListener(Self);
    Reg.Send;
  finally
    Registrar.Free;
  end;
end;

procedure TrnidSpike.FormResize(Sender: TObject);
begin
  Self.UpperInput.Height := (Self.ClientHeight - Self.InputSplitter.Height) div 2;
end;

procedure TrnidSpike.UnregisterClick(Sender: TObject);
var
  Registrar: TIdSipUri;
  Unreg:     TIdSipOutboundUnregister;
begin
  Registrar := TIdSipUri.Create(Self.TargetUri.Text);
  try
    Unreg := Self.UA.UnregisterFrom(Registrar);
    Unreg.AddListener(Self);
    Unreg.Send;
  finally
    Registrar.Free;
  end;
end;

procedure TrnidSpike.OptionsClick(Sender: TObject);
var
  Dest: TIdSipAddressHeader;
  Q:    TIdSipOutboundOptions;
begin
  Dest := TIdSipAddressHeader.Create;
  try
    Dest.Value := Self.TargetUri.Text;
    Q := Self.UA.QueryOptions(Dest);
    Q.AddListener(Self);
    Q.Send;
  finally
    Dest.Free;
  end;
end;

procedure TrnidSpike.ContactUriChange(Sender: TObject);
begin
  Self.UA.Contact.Value := Self.ContactUri.Text;
end;

procedure TrnidSpike.UseAsProxyClick(Sender: TObject);
begin
  Self.UA.HasProxy := Self.UseAsProxy.Checked;
end;

procedure TrnidSpike.PasswordChange(Sender: TObject);
var
  Uri: String;
begin
  Uri := Self.TargetUri.Text;

  if Self.UseLooseRouting.Checked then
    Uri := Uri + ';lr';

  Self.UA.Proxy.Uri := Uri;
end;

procedure TrnidSpike.HostNameChange(Sender: TObject);
var
  I: Integer;
begin
  Self.StopTransports;

  for I := 0 to Self.UA.Dispatcher.Transports.Count - 1 do
    Self.UA.Dispatcher.Transports[I].HostName := Self.HostName.Text;

  Self.UA.HostName := Self.HostName.Text;

  Self.StartTransports;
end;

procedure TrnidSpike.AnswerClick(Sender: TObject);
var
  Answer: String;
  I:      Integer;
  SDP:    TIdSdpPayload;
begin
  if Assigned(Self.LatestSession) then begin
    Self.ResetCounters;

    if (Self.LatestSession.InitialRequest.Body <> '') then begin
      SDP := TIdSdpPayload.CreateFrom(Self.LatestSession.InitialRequest.Body);
      try
        SDP.Origin.Address := Self.Address;
        for I := 0 to SDP.MediaDescriptionCount - 1 do
          SDP.MediaDescriptionAt(I).Connections[0].Address := Self.Address;

        Answer := Self.PayloadProcessor.StartListening(SDP.AsString);

        for I := 0 to Self.PayloadProcessor.StreamCount - 1 do
          Self.PayloadProcessor.Streams[I].AddDataListener(Self);
      finally
        SDP.Free;
      end;

//      Self.AudioPlayer.Play(AnyAudioDevice);

      Self.PayloadProcessor.SetRemoteDescription(Self.LatestSession.InitialRequest.Body);
    end
    else
      Answer := Self.LocalSDP(Self.UA.Dispatcher.Transports[0].Address);

    (Self.LatestSession as TIdSipInboundSession).AcceptCall(Answer, SdpMimeType);
    Self.Answer.Enabled := false;
  end;
end;

procedure TrnidSpike.FromUriChange(Sender: TObject);
begin
  Self.UA.From.Value := Self.FromUri.Text;
end;

end.
