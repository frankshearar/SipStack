unit Spike;

interface

uses
  audioclasses, Classes, Contnrs, Controls, ExtCtrls, Forms, IdDTMFPanel,
  IdRTP, IdRTPTimerQueue, IdSdp, IdSipCore, IdSipMessage, IdSipTransaction,
  IdSipTransport, IdSocketHandle,
  StdCtrls, SyncObjs, SysUtils;

type
  TrnidSpike = class(TForm,
                     IIdRTPDataListener,
                     IIdSipObserver,
                     IIdSipRegistrationListener,
                     IIdSipSessionListener,
                     IIdSipTransportListener,
                     IIdSipTransportSendingListener,
                     IIdSipUserAgentListener)
    UiTimer: TTimer;
    Splitter1: TSplitter;
    IOPanel: TPanel;
    Panel3: TPanel;
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
    Register: TButton;
    RegistrarUri: TEdit;
    LowerInput: TPanel;
    Splitter4: TSplitter;
    OutputText: TMemo;
    procedure ByeClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure InviteClick(Sender: TObject);
    procedure UiTimerTimer(Sender: TObject);
    procedure TextTimerTimer(Sender: TObject);
    procedure InputTextKeyPress(Sender: TObject; var Key: Char);
    procedure BasePortChange(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    CounterLock:    TCriticalSection;
    Lock:           TCriticalSection;
    TextLock:       TCriticalSection;

    AudioPlayer:    TAudioData;
    DataStore:      TStream;
    Dispatch:       TIdSipTransactionDispatcher;
    LowerDTMFPanel: TIdDTMFPanel;
    Media:          TIdSdpPayloadProcessor;
    RTPByteCount:   Integer;
    RunningPort:    Cardinal;
    SendBuffer:     String;
    StopEvent:      TEvent;
    Transports:     TObjectList;
    UA:             TIdSipUserAgentCore;
    UDPByteCount:   Integer;
    UpperDTMFPanel: TIdDTMFPanel;

    function  AddTransport(TransportType: TIdSipTransportClass): TIdSipTransport;
    procedure LogMessage(Msg: TIdSipMessage; Inbound: Boolean);
    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnChanged(Observed: TObject);
    procedure OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                         Receiver: TIdSipTransport);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnEndedSession(Session: TIdSipSession;
                             const Reason: String);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnInboundCall(Session: TIdSipInboundSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure OnPlaybackStopped(Origin: TAudioData);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transport: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Transport: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Transport: TIdSipTransport);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure ProcessPCM(Data: TStream);
    procedure ProcessText(Text: String);
    procedure StartReadingData(const SDP: String);
    procedure StartTransports;
    procedure StopReadingData;
    procedure StopTransports;
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
  Dialogs, Graphics, IdGlobal, IdSipConsts, IdStack;

const
  LocalHostName = '127.0.0.1';

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
var
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.Transports := TObjectList.Create(true);
  Self.RunningPort := IdPORT_SIP;

  Self.UpperDTMFPanel := TIdDTMFPanel.Create(nil);
  Self.UpperDTMFPanel.Align  := alLeft;
  Self.UpperDTMFPanel.Left   := -1;
  Self.UpperDTMFPanel.Parent := Self.UpperInput;
  Self.UpperDTMFPanel.Top    := 0;

  Self.LowerDTMFPanel := TIdDTMFPanel.Create(nil);
  Self.LowerDTMFPanel.Align  := alLeft;
  Self.LowerDTMFPanel.Left   := -1;
  Self.LowerDTMFPanel.Parent := Self.LowerInput;
  Self.LowerDTMFPanel.Top    := 0;

  Self.Lock        := TCriticalSection.Create;
  Self.CounterLock := TCriticalSection.Create;
  Self.TextLock    := TCriticalSection.Create;

  Self.DataStore := TMemoryStream.Create;
  Self.AudioPlayer := TAudioData.Create;
  Self.AudioPlayer.AutoFreeSource := false;
  Self.AudioPlayer.OnStop := Self.OnPlaybackStopped;
  Self.AudioPlayer.SetFormatParameters(afMuLaw, ChannelsMono, 8000, 8);
  Self.AudioPlayer.Assign(Self.DataStore);
//  Self.AudioPlayer.Play(AnyAudioDevice);

  Self.StopEvent := TSimpleEvent.Create;

  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.AddTransport(TIdSipTCPTransport));
  Self.Dispatch.AddTransport(Self.AddTransport(TIdSipUDPTransport));

  Self.Media      := TIdSdpPayloadProcessor.Create;
  Self.Media.Host := (Self.Transports[0] as TIdSipTransport).HostName;
  Self.Media.AddDataListener(Self);
  Self.UpperDTMFPanel.Processor := Self.Media;

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatch;
  Self.UA.AddUserAgentListener(Self);
  Self.UA.AddObserver(Self);
  Self.UA.HostName := (Self.Transports[0] as TIdSipTransport).HostName;
  Self.UA.UserAgentName := 'X-Lite build 1086';

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@'
                   + (Self.Transports[0] as TIdSipTransport).HostName + ':'
                   + IntToStr((Self.Transports[0] as TIdSipTransport).Bindings[0].Port);
    Self.UA.Contact := Contact;
  finally
    Contact.Free;
  end;

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'sip:franks@' + (Self.Transports[0] as TIdSipTransport).HostName;
    Self.UA.From := From;
  finally
    From.Free;
  end;

  try
    BasePort.Text := IntToStr((Self.Transports[0] as TIdSipTransport).Bindings[0].Port);
    Self.StartTransports;
  except
    on EIdCouldNotBindSocket do
      ShowMessage('Something''s hogged the SIP port '
                + '(' + IntToStr((Self.Transports[0] as TIdSipTransport).Bindings[0].Port) + ') - '
                + 'kill it and restart this');
  end;
end;

destructor TrnidSpike.Destroy;
begin
  Self.StopTransports;

  Self.UA.Free;
  Self.Dispatch.Free;
  Self.Media.Free;

  Self.TextLock.Free;
  Self.CounterLock.Free;
  Self.Lock.Free;

  // If no data at all has arrived we stall here.
  Self.AudioPlayer.Stop;
  Self.StopEvent.WaitFor(1000);
  Self.AudioPlayer.Free;

  Self.DataStore.Free;
  Self.StopEvent.Free;
  Self.LowerDTMFPanel.Free;
  Self.UpperDTMFPanel.Free;

  Self.Transports.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

function TrnidSpike.AddTransport(TransportType: TIdSipTransportClass): TIdSipTransport;
var
  Binding: TIdSocketHandle;
begin
  Result := TransportType.Create(IdPORT_SIP);
  Self.Transports.Add(Result);
{
  if (GStack.LocalAddress <> LocalHostName) then begin
    Binding      := Result.Bindings.Add;
    Binding.IP   := GStack.LocalAddress;
    Binding.Port := RunningPort;
    Result.HostName := Binding.IP;
  end
  else
}
    Result.HostName := LocalHostName;

  Binding      := Result.Bindings.Add;
  Binding.IP   := LocalHostName;
  Binding.Port := RunningPort;
  Result.AddTransportListener(Self);
  Result.AddTransportSendingListener(Self);
end;

procedure TrnidSpike.LogMessage(Msg: TIdSipMessage; Inbound: Boolean);
begin
  Self.Lock.Acquire;
  try
    if Inbound then
      Self.Log.Lines.Add('<<<< ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now))
    else
      Self.Log.Lines.Add('>>>> ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now));

    Self.Log.Lines.Add(Msg.AsString);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                               Response: TIdSipResponse);
begin
end;

procedure TrnidSpike.OnChanged(Observed: TObject);
begin
  Self.SessionCounter.Caption := IntToStr((Observed as TIdSipUserAgentCore).SessionCount);
end;

procedure TrnidSpike.OnDroppedUnmatchedResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('Dropped unmatched response: ' + Response.Description);
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.OnEstablishedSession(Session: TIdSipSession);
begin
  Self.Media.RemoteSessionDescription := Session.PayloadProcessor.RemoteSessionDescription;
end;

procedure TrnidSpike.OnEndedSession(Session: TIdSipSession;
                                    const Reason: String);
begin
  Self.Lock.Acquire;
  try
    Self.Log.Lines.Add('Session ended: ' + Reason);
  finally
    Self.Lock.Release;
  end;
  Self.StopReadingData;
  Session.PayloadProcessor.RemoveDataListener(Self.UpperDTMFPanel);
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

procedure TrnidSpike.OnFailure(RegisterAgent: TIdSipRegistration;
                               CurrentBindings: TIdSipContacts;
                               const Reason: String);
begin
end;

procedure TrnidSpike.OnInboundCall(Session: TIdSipInboundSession);
begin
  if (Session.Invite.ContentLength > 0) then
    Self.StartReadingData(Session.Invite.Body);

  Session.AddSessionListener(Self);
  Session.AcceptCall(Self.Media.LocalSessionDescription, Self.Media.MediaType);

  Session.PayloadProcessor.AddDataListener(Self.UpperDTMFPanel);
end;

procedure TrnidSpike.OnModifiedSession(Session: TIdSipSession;
                                       Invite: TIdSipRequest);
begin
end;

procedure TrnidSpike.OnNewData(Data: TIdRTPPayload;
                               Binding: TIdSocketHandle);
var
  S: TStringStream;
begin
  if (Lowercase(Data.Name) = Lowercase(PCMMuLawEncoding)) then begin
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

procedure TrnidSpike.OnPlaybackStopped(Origin: TAudioData);
begin
  Self.StopEvent.SetEvent;
end;

procedure TrnidSpike.OnReceiveRequest(Request: TIdSipRequest;
                                      Transport: TIdSipTransport);
begin
  Self.LogMessage(Request, true);
end;

procedure TrnidSpike.OnReceiveResponse(Response: TIdSipResponse;
                                       Transport: TIdSipTransport);
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

procedure TrnidSpike.OnSendRequest(Request: TIdSipRequest;
                                   Transport: TIdSipTransport);
begin
  Self.LogMessage(Request, false);
end;

procedure TrnidSpike.OnSendResponse(Response: TIdSipResponse;
                                    Transport: TIdSipTransport);
begin
  Self.LogMessage(Response, false);
end;

procedure TrnidSpike.OnSuccess(RegisterAgent: TIdSipRegistration;
                               CurrentBindings: TIdSipContacts);
begin
end;

procedure TrnidSpike.ProcessPCM(Data: TStream);
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

procedure TrnidSpike.ProcessText(Text: String);
begin
  Self.Lock.Acquire;
  try
    Self.OutputText.Text := Self.OutputText.Text + Text; 
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpike.StartReadingData(const SDP: String);
begin
  Self.Invite.Enabled := false;
  Self.RTPByteCount   := 0;
  Self.UDPByteCount   := 0;

  Self.Media.StartListening(SDP);
end;

procedure TrnidSpike.StartTransports;
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    (Self.Transports[I] as TIdSipTransport).Start;
end;

procedure TrnidSpike.StopReadingData;
begin
  Self.Media.StopListening;
  Self.Invite.Enabled := true;
end;

procedure TrnidSpike.StopTransports;
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    (Self.Transports[I] as TIdSipTransport).Stop;
end;

//* TrnidSpike Published methods ***********************************************

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
  Address := (Self.Transports[0] as TIdSipTransport).Bindings[0].IP;

  Target := TIdSipToHeader.Create;
  try
    Target.Value := Self.TargetUri.Text;

    SDP := 'v=0'#13#10
         + 'o=franks 123456 123456 IN IP4 ' + Address + #13#10
         + 's=-'#13#10
         + 'c=IN IP4 ' + Address + #13#10
         + 't=0 0'#13#10
         + 'm=audio 8000 RTP/AVP 0'#13#10
         + 'm=audio 8002 RTP/AVP 96'#13#10
         + 'a=rtpmap:96 telephone-event/8000'#13#10
         + 'a=fmtp:101 0-16'#13#10
         + 'm=text 8004 RTP/AVP 97'#13#10
         + 'a=rtpmap:97 T140/1000'#13#10;


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

procedure TrnidSpike.TextTimerTimer(Sender: TObject);
var
  Text: TIdRTPT140Payload;
begin
  Self.TextLock.Acquire;
  try
    if (Self.SendBuffer <> '') then begin
      Text := Self.Media.Profile.EncodingFor(T140Encoding + '/' + IntToStr(T140ClockRate)).Clone as TIdRTPT140Payload;
      try
        Text.Block := Self.SendBuffer;
//        Self.Media.SessionFor(Text).SendData(Text);
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
  I, J: Integer;
begin
  Self.StopTransports;

  for I := 0 to Self.Transports.Count - 1 do
    for J := 0 to (Self.Transports[I] as TIdSipTransport).Bindings.Count - 1 do
      (Self.Transports[I] as TIdSipTransport).Bindings[J].Port := StrToInt(BasePort.Text);

  Self.StartTransports;
end;

procedure TrnidSpike.RegisterClick(Sender: TObject);
var
  Registrar: TIdSipUri;
begin
  Registrar := TIdSipUri.Create(Self.RegistrarUri.Text);
  try
    Self.UA.RegisterWith(Registrar);
  finally
    Registrar.Free;
  end;
end;

procedure TrnidSpike.FormResize(Sender: TObject);
begin
  Self.UpperInput.Height := (Self.ClientHeight - Self.InputSplitter.Height) div 2;
end;

end.
