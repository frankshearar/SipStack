unit Spike;

interface

uses
  audioclasses, Classes, Controls, ExtCtrls, Forms, IdRTP, IdSdp, IdSipCore,
  IdSipMessage, IdSipTransaction, IdSipTransport, IdSocketHandle, StdCtrls,
  SyncObjs;

type
  TIdDTMFPanel = class;

  TrnidSpike = class(TForm,
                     IIdRTPDataListener,
                     IIdSipObserver,
                     IIdSipSessionListener,
                     IIdSipTransportListener,
                     IIdSipTransportSendingListener)
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
    OutputText: TMemo;
    Splitter2: TSplitter;
    UpperInput: TPanel;
    Splitter3: TSplitter;
    InputText: TMemo;
    TextTimer: TTimer;
    BasePort: TEdit;
    procedure ByeClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure InviteClick(Sender: TObject);
    procedure UiTimerTimer(Sender: TObject);
    procedure TextTimerTimer(Sender: TObject);
    procedure InputTextKeyPress(Sender: TObject; var Key: Char);
    procedure BasePortChange(Sender: TObject);
  private
    CounterLock:  TCriticalSection;
    Lock:         TCriticalSection;
    TextLock:     TCriticalSection;

    AudioPlayer:  TAudioData;
    DataStore:    TStream;
    Dispatch:     TIdSipTransactionDispatcher;
    DTMFPanel:    TIdDTMFPanel;
    Media:        TIdSdpPayloadProcessor;
    RTPByteCount: Integer;
    SendBuffer:   String;
    StopEvent:    TEvent;
    TransportUDP: TIdSipTransport;
    UA:           TIdSipUserAgentCore;
    UDPByteCount: Integer;

    procedure LogMessage(Msg: TIdSipMessage);
    procedure OnChanged(Observed: TObject);
    procedure OnEstablishedSession(Session: TIdSipSession);
    procedure OnEndedSession(Session: TIdSipSession);
    procedure OnModifiedSession(Session: TIdSipSession;
                                Invite: TIdSipRequest);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure OnNewSession(Session: TIdSipSession);
    procedure OnPlaybackStopped(Origin: TAudioData);
    procedure OnReceiveRequest(const Request: TIdSipRequest;
                               const Transport: TIdSipTransport);
    procedure OnReceiveResponse(const Response: TIdSipResponse;
                                const Transport: TIdSipTransport);
    procedure OnSendRequest(const Request: TIdSipRequest;
                            const Transport: TIdSipTransport);
    procedure OnSendResponse(const Response: TIdSipResponse;
                             const Transport: TIdSipTransport);
    procedure ProcessPCM(Data: TStream);
    procedure ProcessText(Text: String);
    procedure StartReadingData(const SDP: String);
    procedure StopReadingData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TColourButton = class(TButton)
  public
    property Color;
  end;

  TIdDTMFButtonArray = array[DTMF0..DTMFFlash] of TColourButton;

  // Still needs: flashing of buttons; maybe playing of tones, too, on receipt
  // of DTMF
  TIdDTMFPanel = class(TPanel,
                       IIdRTPDataListener)
  private
    Buttons:          TIdDTMFButtonArray;
    ButtonHeight:     Integer;
    CurrentRowHeight: Integer;
    fSession:         TIdRTPSession;

    procedure AddRow(Buttons: array of TColourButton);
    function  CreateButton(Name: String;
                           Event: TNotifyEvent): TColourButton;
    procedure ResizeButtons;
    procedure DoOnResize(Sender: TObject);
    procedure Flash(Event: TIdRTPTelephoneEventPayload);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure SendDTMF(Event: Byte);
    procedure SendDTMF0(Sender: TObject);
    procedure SendDTMF1(Sender: TObject);
    procedure SendDTMF2(Sender: TObject);
    procedure SendDTMF3(Sender: TObject);
    procedure SendDTMF4(Sender: TObject);
    procedure SendDTMF5(Sender: TObject);
    procedure SendDTMF6(Sender: TObject);
    procedure SendDTMF7(Sender: TObject);
    procedure SendDTMF8(Sender: TObject);
    procedure SendDTMF9(Sender: TObject);
    procedure SendDTMFA(Sender: TObject);
    procedure SendDTMFB(Sender: TObject);
    procedure SendDTMFC(Sender: TObject);
    procedure SendDTMFD(Sender: TObject);
    procedure SendDTMFFlash(Sender: TObject);
    procedure SendDTMFHash(Sender: TObject);
    procedure SendDTMFStar(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property Session: TIdRTPSession read fSession write fSession;
  end;

const
  AudioFile = 'dump.wav';

var
  rnidSpike: TrnidSpike;

implementation

{$R *.dfm}

uses
  Dialogs, Graphics, IdGlobal, IdSipConsts, IdSipHeaders, IdStack, SysUtils;

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
const
  LocalHostName = '127.0.0.1';
var
  Binding:     TIdSocketHandle;
  Contact:     TIdSipContactHeader;
  From:        TIdSipFromHeader;
  RunningPort: Cardinal;
begin
  inherited Create(AOwner);

  RunningPort := IdPORT_SIP;

  Self.DTMFPanel := TIdDTMFPanel.Create(nil);
  Self.DTMFPanel.Align  := alLeft;
  Self.DTMFPanel.Left   := -1;
  Self.DTMFPanel.Parent := Self.UpperInput;
  Self.DTMFPanel.Top    := 0;

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

  Self.StopEvent   := TSimpleEvent.Create;

  Self.TransportUDP := TIdSipUdpTransport.Create(IdPORT_SIP);
  if (GStack.LocalAddress <> LocalHostName) then begin
    Binding      := Self.TransportUDP.Bindings.Add;
    Binding.IP   := GStack.LocalAddress;
    Binding.Port := RunningPort;
    Self.TransportUDP.HostName := Binding.IP;
  end
  else
    Self.TransportUDP.HostName := LocalHostName;

  Binding      := Self.TransportUDP.Bindings.Add;
  Binding.IP   := LocalHostName;
  Binding.Port := RunningPort;

  Self.Media      := TIdSdpPayloadProcessor.Create;
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
    Contact.Value := 'sip:franks@'
                   + Self.TransportUDP.HostName + ':'
                   + IntToStr(Self.TransportUDP.Bindings[0].Port);
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
    BasePort.Text := IntToStr(Self.TransportUDP.Bindings[0].Port);
    Self.TransportUDP.Start;
  except
    on EIdCouldNotBindSocket do
      ShowMessage('Something''s hogged the SIP port '
                + '(' + IntToStr(Self.TransportUDP.Bindings[0].Port) + ') - '
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

  Self.TextLock.Free;
  Self.CounterLock.Free;
  Self.Lock.Free;

  // If no data at all has arrived we stall here.
  Self.AudioPlayer.Stop;
  Self.StopEvent.WaitFor(10000);
  Self.AudioPlayer.Free;

  Self.DataStore.Free;
  Self.StopEvent.Free;
  Self.DTMFPanel.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

procedure TrnidSpike.LogMessage(Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpike.OnChanged(Observed: TObject);
begin
  Self.SessionCounter.Caption := IntToStr((Observed as TIdSipUserAgentCore).SessionCount);
end;

procedure TrnidSpike.OnEstablishedSession(Session: TIdSipSession);
begin
end;

procedure TrnidSpike.OnEndedSession(Session: TIdSipSession);
begin
  Self.StopReadingData;
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

procedure TrnidSpike.OnNewSession(Session: TIdSipSession);
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

procedure TrnidSpike.StopReadingData;
begin
  Self.Media.StopListening;
  Self.Invite.Enabled := true;
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
  Address := Self.TransportUDP.Bindings[0].IP;

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

//*****************************************************************************
//* TIdDTMFPanel                                                              *
//*****************************************************************************
//* TIdDTMFPanel Public methods ***********************************************

constructor TIdDTMFPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ButtonHeight     := 25;
  Self.Height           := 120;
  Self.OnResize         := Self.DoOnResize;
  Self.Width            := 96;

  Self.Buttons[DTMF0]     := Self.CreateButton('0',     Self.SendDTMF0);
  Self.Buttons[DTMF1]     := Self.CreateButton('1',     Self.SendDTMF1);
  Self.Buttons[DTMF2]     := Self.CreateButton('2',     Self.SendDTMF2);
  Self.Buttons[DTMF3]     := Self.CreateButton('3',     Self.SendDTMF3);
  Self.Buttons[DTMF4]     := Self.CreateButton('4',     Self.SendDTMF4);
  Self.Buttons[DTMF5]     := Self.CreateButton('5',     Self.SendDTMF5);
  Self.Buttons[DTMF6]     := Self.CreateButton('6',     Self.SendDTMF6);
  Self.Buttons[DTMF7]     := Self.CreateButton('7',     Self.SendDTMF7);
  Self.Buttons[DTMF8]     := Self.CreateButton('8',     Self.SendDTMF8);
  Self.Buttons[DTMF9]     := Self.CreateButton('9',     Self.SendDTMF9);
  Self.Buttons[DTMFA]     := Self.CreateButton('A',     Self.SendDTMFA);
  Self.Buttons[DTMFB]     := Self.CreateButton('B',     Self.SendDTMFB);
  Self.Buttons[DTMFC]     := Self.CreateButton('C',     Self.SendDTMFC);
  Self.Buttons[DTMFD]     := Self.CreateButton('D',     Self.SendDTMFD);
  Self.Buttons[DTMFStar]  := Self.CreateButton('*',     Self.SendDTMFStar);
  Self.Buttons[DTMFHash]  := Self.CreateButton('#',     Self.SendDTMFHash);
  Self.Buttons[DTMFFlash] := Self.CreateButton('Flash', Self.SendDTMFFlash);

  Self.ResizeButtons;
end;

destructor TIdDTMFPanel.Destroy;
var
  I: Integer;
begin
  for I := Low(Self.Buttons) to High(Self.Buttons) do
    Self.Buttons[I].Free;


  inherited Destroy;
end;

//* TIdDTMFPanel Private methods ***********************************************

procedure TIdDTMFPanel.AddRow(Buttons: array of TColourButton);
var
  I:      Integer;
  XCoord: Integer;
  Width:  Integer;
begin
  XCoord := 0;
  Width := Self.Width div Length(Buttons) + 1;
  for I := Low(Buttons) to High(Buttons) do begin
    Buttons[I].Height := Self.ButtonHeight;
    Buttons[I].Left   := XCoord;
    Buttons[I].Parent := Self; // Yes, yes, this is wasteful
    Buttons[I].Top    := Self.CurrentRowHeight;
    Buttons[I].Width  := Width;
    Inc(XCoord, Buttons[I].Width - 1);
  end;

  // Adjust the last button to take up all remaining horizontal space
  Buttons[High(Buttons)].Width := Self.Width - Buttons[High(Buttons)].Left + 1;

  Inc(Self.CurrentRowHeight, Buttons[High(Buttons)].Height - 1);
end;

function TIdDTMFPanel.CreateButton(Name: String;
                                   Event: TNotifyEvent): TColourButton;
begin
  Result := TColourButton.Create(nil);
  Result.Caption := Name;
  Result.OnClick := Event;
end;

procedure TIdDTMFPanel.ResizeButtons;
begin
  Self.CurrentRowHeight := 0;
  Self.ButtonHeight := Self.Height div 5 + 1;

  Self.AddRow([Self.Buttons[DTMF1],
               Self.Buttons[DTMF2],
               Self.Buttons[DTMF3],
               Self.Buttons[DTMFA]]);
  Self.AddRow([Self.Buttons[DTMF4],
               Self.Buttons[DTMF5],
               Self.Buttons[DTMF6],
               Self.Buttons[DTMFB]]);
  Self.AddRow([Self.Buttons[DTMF7],
               Self.Buttons[DTMF8],
               Self.Buttons[DTMF9],
               Self.Buttons[DTMFC]]);
  Self.AddRow([Self.Buttons[DTMFStar],
               Self.Buttons[DTMF0],
               Self.Buttons[DTMFHash],
               Self.Buttons[DTMFD]]);
  Self.AddRow([Self.Buttons[DTMFFlash]]);

  // Adjust the last row to take up all remaining vertical space
  if (Self.Buttons[DTMFFlash].Top + Self.Buttons[DTMFFlash].Height < Self.Height) then
    Self.Buttons[DTMFFlash].Height := Self.Height - Self.Buttons[DTMFFlash].Top + 1;

  Self.Constraints.MinHeight := 5*13;
  Self.Constraints.MinWidth  := 4*10;
end;

procedure TIdDTMFPanel.DoOnResize(Sender: TObject);
begin
  Self.ResizeButtons;
end;

procedure TIdDTMFPanel.Flash(Event: TIdRTPTelephoneEventPayload);
var
  Button: TColourButton;
begin
  // We've received a DTMF event. Make the appropriate button flash
  // for the specified amount of time.
  Button := Self.Buttons[Event.Event];
  Button.Color := clBlack xor clWhite;

  // wait Duration milliseconds

  if Event.IsEnd then
    Button.Color := clBlack xor clWhite;
  // what do we do if the ending packet (IsEnd is true after a series of
  // non-end) never arrives? 
end;

procedure TIdDTMFPanel.OnNewData(Data: TIdRTPPayload;
                                 Binding: TIdSocketHandle);
begin
  // TODO: Could this indicate the need for a Visitor pattern?
  if Data is TIdRTPTelephoneEventPayload then
    Self.Flash(Data as TIdRTPTelephoneEventPayload);
end;

procedure TIdDTMFPanel.SendDTMF(Event: Byte);
var
  TE: TIdRTPTelephoneEventPayload;
begin
  if Assigned(Session) then begin
    TE := Session.Profile.EncodingFor(TelephoneEventEncoding + '/8000').Clone as TIdRTPTelephoneEventPayload;
    try
      TE.Event    := Event;
      TE.Duration := 100;
      
      Session.SendData(TE);
    finally
      TE.Free;
    end;
  end;
end;

procedure TIdDTMFPanel.SendDTMF0(Sender: TObject);
begin
  Self.SendDTMF(DTMF0);
end;

procedure TIdDTMFPanel.SendDTMF1(Sender: TObject);
begin
  Self.SendDTMF(DTMF1);
end;

procedure TIdDTMFPanel.SendDTMF2(Sender: TObject);
begin
  Self.SendDTMF(DTMF2);
end;

procedure TIdDTMFPanel.SendDTMF3(Sender: TObject);
begin
  Self.SendDTMF(DTMF3);
end;

procedure TIdDTMFPanel.SendDTMF4(Sender: TObject);
begin
  Self.SendDTMF(DTMF4);
end;

procedure TIdDTMFPanel.SendDTMF5(Sender: TObject);
begin
  Self.SendDTMF(DTMF5);
end;

procedure TIdDTMFPanel.SendDTMF6(Sender: TObject);
begin
  Self.SendDTMF(DTMF6);
end;

procedure TIdDTMFPanel.SendDTMF7(Sender: TObject);
begin
  Self.SendDTMF(DTMF7);
end;

procedure TIdDTMFPanel.SendDTMF8(Sender: TObject);
begin
  Self.SendDTMF(DTMF8);
end;

procedure TIdDTMFPanel.SendDTMF9(Sender: TObject);
begin
  Self.SendDTMF(DTMF9);
end;

procedure TIdDTMFPanel.SendDTMFA(Sender: TObject);
begin
  Self.SendDTMF(DTMFA);
end;

procedure TIdDTMFPanel.SendDTMFB(Sender: TObject);
begin
  Self.SendDTMF(DTMFB);
end;

procedure TIdDTMFPanel.SendDTMFC(Sender: TObject);
begin
  Self.SendDTMF(DTMFC);
end;

procedure TIdDTMFPanel.SendDTMFD(Sender: TObject);
begin
  Self.SendDTMF(DTMFD);
end;

procedure TIdDTMFPanel.SendDTMFFlash(Sender: TObject);
begin
  Self.SendDTMF(DTMFFlash);
end;

procedure TIdDTMFPanel.SendDTMFHash(Sender: TObject);
begin
  Self.SendDTMF(DTMFHash);
end;

procedure TIdDTMFPanel.SendDTMFStar(Sender: TObject);
begin
  Self.SendDTMF(DTMFStar);
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
  I: Integer;
begin
  Self.TransportUDP.Stop;

  for I := 0 to Self.TransportUDP.Bindings.Count - 1 do
    Self.TransportUDP.Bindings[I].Port := StrToInt(BasePort.Text);
  Self.TransportUDP.Start;
end;

end.