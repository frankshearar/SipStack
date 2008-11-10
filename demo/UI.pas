{
  (c) 2008 Technology Directorate, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit UI;

interface

uses
  Classes, Controls, ExtCtrls, Forms, Messages, IdConnectionBindings, IdRtp,
  IdSdp, IdSipMessage, IdSipStackInterface, IdTimerQueue, StdCtrls, Windows;

type
  TUIForm = class(TForm, IIdSdpMediaListener)
    Panel1: TPanel;
    Call: TButton;
    HangUp: TButton;
    PartyPanel: TPanel;
    CalleeLog: TMemo;
    PartySplitter: TSplitter;
    Log: TMemo;
    CallerLog: TMemo;
    MainSplitter: TSplitter;
    CallerSendsMedia: TButton;
    CalleeSendsMedia: TButton;
    procedure CallClick(Sender: TObject);
    procedure HangUpClick(Sender: TObject);
    procedure PartyPanelResize(Sender: TObject);
    procedure CallerSendsMediaClick(Sender: TObject);
    procedure CalleeSendsMediaClick(Sender: TObject);
  private
    Callee:      TIdSipHandle;
    CalleeMedia: TIdSDPMultimediaSession;
    Caller:      TIdSipHandle;
    CallerMedia: TIdSDPMultimediaSession;
    From:        TIdSipAddressHeader;
    Profile:     TIdRTPProfile;
    Timer:       TIdTimerQueue;
    Signaller:   HWND;
    Stack:       TIdSipStackInterface;
    T140PT:      TIdRTPPayloadType;

    function  BasicSdp: String;
    procedure CallEnded(Data: TIdCallEndedData);
    procedure DisableMediaButton(H: TIdSipHandle);
    procedure EnableMediaButton(H: TIdSipHandle; Enabled: Boolean = true);
    procedure EstablishCall(Data: TIdEstablishedSessionData);
    function  IsStackMessage(Msg: TMessage): Boolean;
    procedure ListenTo(Proc: TIdSDPMultimediaSession);
    procedure LogEvent(Data: String; Party: TIdSipHandle);
    function  Media(H: TIdSipHandle): TIdSDPMultimediaSession;
    procedure OnData(Stream: TIdSdpBaseMediaStream;
                     Chunk: TStream;
                     Format: String;
                     Binding: TIdConnectionBindings);
    function  Party(H: TIdSipHandle): String;
    procedure ReceiveCall(Data: TIdInboundCallData);
    procedure ReceiveMessage(var Msg: TMessage);
    procedure ReceiveNotify(Event: Cardinal;
                            Data: TIdSipStackInterfaceEventMethod);
    procedure SendMedia(Proc: TIdSDPMultimediaSession);
    procedure StopListeningTo(Proc: TIdSDPMultimediaSession);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  UIForm: TUIForm;

implementation

{$R *.dfm}

uses
  IdSipTransport, IdSipTcpTransport, IdSipUdpTransport, SysUtils;

const
  MAX_HOSTNAME_LEN    = 128;
  MAX_DOMAIN_NAME_LEN = 128;
  MAX_SCOPE_ID_LEN    = 256;

type
  TIPAddressString = array[0..15] of AnsiChar;
  PIPAddressString = ^TIPAddressString;
  PIPAddrString = ^TIPAddrString;
  TIPAddrString = packed record
    Next      : PIPAddrString;
    IPAddress : TIPAddressString;
    IPMask    : TIPAddressString;
    Context   : DWord;
  end;
  PFixedInfo = ^TFixedInfo;
  TFixedInfo = packed record
    HostName         : packed array[0..(MAX_HOSTNAME_LEN+4)-1] of AnsiChar;
    DomainName       : packed array[0..(MAX_DOMAIN_NAME_LEN+4)-1] of AnsiChar;
    CurrentDNSServer : PIPAddrString;
    DNSServerList    : TIPAddrString;
    NodeType         : LongWord;
    ScopeId          : packed array[0..(MAX_SCOPE_ID_LEN+4)-1] of AnsiChar;
    EnableRouting    : LongWord;
    EnableProxy      : LongWord;
    EnableDNS        : LongWord;
  end;

//******************************************************************************
//* Unit Private functions & procedures                                        *
//******************************************************************************

function GetNetworkParams(pFixedInfo: PFixedInfo; var pOutBufLen: LongWord): DWord; stdcall; external 'iphlpapi.dll' Name 'GetNetworkParams';

function GetDefaultDNSServer: String;
var
  BufferLength: LongWord;
  FixedInfo:    PFixedInfo;
begin
  Result       := '0.0.0.0';
  FixedInfo    := nil;
  BufferLength := 0;

  if GetNetworkParams(FixedInfo, BufferLength) = ERROR_BUFFER_OVERFLOW then begin
    GetMem(FixedInfo, BufferLength);
    try
      if GetNetworkParams(FixedInfo, BufferLength) = ERROR_SUCCESS then
        Result := FixedInfo^.DNSServerList.IPAddress;
    finally
      FreeMem(FixedInfo);
    end;
  end;
end;

//******************************************************************************
//* TUIForm                                                                    *
//******************************************************************************
//* TUIForm Public methods *****************************************************

constructor TUIForm.Create(AOwner: TComponent);
var
  Conf: TStrings;
  T:    TIdRTPT140Payload;
begin
  inherited Create(AOwner);

  TIdSipTransportRegistry.RegisterTransportType(TcpTransport, TIdSipTcpTransport);
  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipUdpTransport);

  // This demo uses RFC 4103 real-time text. That's not in the profile described
  // in RFC 3551, wo We tell the profile what kind of RTP we're going to be
  // using.
  Self.Profile := TIdAudioVisualProfile.Create;
  T := TIdRTPT140Payload.Create;
  try
    Self.T140PT  := Self.Profile.FirstFreePayloadType;
    Self.Profile.AddEncoding(T, Self.T140PT);
  finally
    T.Free;
  end;

  // Sometimes one can omit the setting of IsOffer: it only really becomes
  // important for TCP streams, described in RFC 4145.
  Self.CalleeMedia := TIdSDPMultimediaSession.Create(Self.Profile);
  Self.CalleeMedia.IsOffer := false;

  Self.CallerMedia := TIdSDPMultimediaSession.Create(Self.Profile);
  Self.CallerMedia.IsOffer := true;

  Self.From        := TIdSipFromHeader.Create;
  Self.From.Value  := 'sip:local@localhost';

  Self.Signaller := Classes.AllocateHWnd(Self.ReceiveMessage);

  Conf := TStringList.Create;
  try
    // This is an absolute minimal configuration. See TIdSipUserAgent's class
    // comments (or doc/directives.txt) for full details.
    Conf.Add('Listen: UDP 127.0.0.1:5060');
    Conf.Add(Format('NameServer: %s:53', [GetDefaultDNSServer]));

    Self.Timer := TIdThreadedTimerQueue.Create(false);
    Self.Stack := TIdSipStackInterface.Create(Self.Signaller, Self.Timer, Conf);
    Self.Stack.Resume;
  finally
    Conf.Free;
  end;
end;

destructor TUIForm.Destroy;
begin
  Classes.DeallocateHWnd(Self.Signaller);

  Self.Timer.Terminate;
  Self.Stack.Free;
  Self.From.Free;
  Self.CallerMedia.Free;
  Self.CalleeMedia.Free;
  Self.Profile.Free;

  TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited Destroy;
end;

//* TUIForm Private methods ****************************************************

function TUIForm.BasicSdp: String;
begin
  Result := Format('v=0'#13#10
                 + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
                 + 's=-'#13#10
                 + 't=0 0'#13#10
                 + 'm=text 8000 RTP/AVP %d'#13#10
                 + 'c=IN IP4 127.0.0.1'#13#10
                 + 'a=rtpmap:96 t140/1000'#13#10, [Self.T140PT]);
end;

procedure TUIForm.CallEnded(Data: TIdCallEndedData);
var
  Media: TIdSDPMultimediaSession;
begin
  Self.DisableMediaButton(Data.Handle);

  Media := Self.Media(Data.Handle);

  Media.StopListening;
  Self.StopListeningTo(Media);
end;

procedure TUIForm.DisableMediaButton(H: TIdSipHandle);
begin
  Self.EnableMediaButton(H, false);
end;

procedure TUIForm.EnableMediaButton(H: TIdSipHandle; Enabled: Boolean = true);
begin
  if (Self.Caller = H) then
    Self.CallerSendsMedia.Enabled := Enabled
  else
    Self.CalleeSendsMedia.Enabled := Enabled;
end;

procedure TUIForm.EstablishCall(Data: TIdEstablishedSessionData);
begin
  // At this point the call represented by Data.Handle is fully established.

  // Only the caller needs its remote description set: the callee (a) receives
  // its remote description when it receives the call request and (b) sets its
  // local description when it answers the call.
  if (Self.Caller = Data.Handle) then
    Self.CallerMedia.SetRemoteDescription(Data.RemoteSessionDescription);

  Self.EnableMediaButton(Data.Handle);

  Self.LogEvent(Format('Established call from %s', [Data.RemoteParty.Value]), Data.Handle);
end;

function TUIForm.IsStackMessage(Msg: TMessage): Boolean;
begin
  Result := ((Msg.Msg >= CM_BASE) and (Msg.Msg <= CM_LAST));
end;

procedure TUIForm.ListenTo(Proc: TIdSDPMultimediaSession);
var
  I: Integer;
begin
  for I := 0 to Proc.StreamCount - 1 do
    Proc.Streams[I].AddDataListener(Self);
end;

procedure TUIForm.LogEvent(Data: String; Party: TIdSipHandle);
begin
  Self.Log.Lines.Add(Format('%s (%s)', [Data, Self.Party(Party)]));
end;

function TUIForm.Media(H: TIdSipHandle): TIdSDPMultimediaSession;
begin
  if (H = Self.Caller) then
    Result := Self.CallerMedia
  else
    Result := Self.CalleeMedia;
end;

procedure TUIForm.OnData(Stream: TIdSdpBaseMediaStream;
                         Chunk: TStream;
                         Format: String;
                         Binding: TIdConnectionBindings);
var
  S:   TStringStream;
  Log: TMemo;
begin
  if (Self.CalleeMedia.Streams[0] = Stream) then
    Log := Self.CalleeLog
  else
    Log := Self.CallerLog;

  S := TStringStream.Create('');
  try
    S.CopyFrom(Chunk, 0);

    Log.Lines.Add(S.DataString);
  finally
    S.Free;
  end;
end;

function TUIForm.Party(H: TIdSipHandle): String;
begin
  if (H = Self.Caller) then
    Result := 'Caller'
  else
    Result := 'Callee';
end;

procedure TUIForm.ReceiveCall(Data: TIdInboundCallData);
begin
  // We've received a call. We can choose to accept the call, or reject it.
  // If we wish to accept the call, we must establish our media streams BEFORE
  // accepting the call, so as to (a) ensure that we can actually start up our
  // sockets and (b) give the caller the proper socket descriptions.
  //
  // (TIdSDPMultimediaSession TRIES to use the ports you give it in
  // LocalSessionDescription but, if those ports are already used, will attempt
  // to open the next higher port, repeating the process until either it opens a
  // port (or ports), or runs out of available ports. In the latter case, the
  // streams are simply refused, by setting their port numbers to 0. See
  // TIdSdpBaseMediaStream.BindListeningStreams.)

  Self.Callee := Data.Handle;
  Self.CalleeMedia.StartListening(BasicSdp);
  Self.CalleeMedia.SetRemoteDescription(Data.RemoteSessionDescription);
  Self.ListenTo(Self.CalleeMedia);

  Self.Stack.AnswerCall(Self.Callee, Self.CalleeMedia.LocalSessionDescription, Self.CalleeMedia.MimeType);

  Self.LogEvent(Format('Received call from %s', [Data.RemoteParty.Value]), Data.Handle);
end;

procedure TUIForm.ReceiveMessage(var Msg: TMessage);
var
  Event: TIdSipEventMessage;
begin
  if Self.IsStackMessage(Msg) then begin
    Event := TIdSipEventMessage(Msg);
    try
      Self.ReceiveNotify(Event.Event, Event.Data);
    finally
      Event.Data.Free;
    end;
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(Self.Signaller, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TUIForm.ReceiveNotify(Event: Cardinal;
                                Data: TIdSipStackInterfaceEventMethod);
begin
  try
    case Event of
      CM_CALL_ENDED:          Self.CallEnded(Data.Data as TIdCallEndedData);
      CM_CALL_ESTABLISHED:    Self.EstablishCall(Data.Data as TIdEstablishedSessionData);
      CM_CALL_REQUEST_NOTIFY: Self.ReceiveCall(Data.Data as TIdInboundCallData);
    end;
  except
    // Something really bad happened.
    on E: Exception do
      Self.LogEvent(Format('Something really bad happened in TUIForm.ReceiveNotify: %s: %s', [E.ClassName, E.Message]), Data.Data.Handle);
  end;
end;

procedure TUIForm.SendMedia(Proc: TIdSDPMultimediaSession);
var
  TestData: TStringStream;
begin
  TestData := TStringStream.Create('test');
  try
    Proc.Streams[0].SendData(TestData, IntToStr(Self.T140PT));
  finally
    TestData.Free;
  end;
end;

procedure TUIForm.StopListeningTo(Proc: TIdSDPMultimediaSession);
var
  I: Integer;
begin
  for I := 0 to Proc.StreamCount - 1 do
    Proc.Streams[I].RemoveDataListener(Self);
end;

//* TUIForm Published methods **************************************************

procedure TUIForm.CallClick(Sender: TObject);
begin
  // We must set up our media BEFORE making the call. See ReceiveCall for
  // detailed reasons.

  Self.CallerMedia.StartListening(BasicSdp);
  Self.ListenTo(Self.CallerMedia);

  // This call is (hopefully obviously) a loopback call - what's called a hairpin.
  Self.Caller := Self.Stack.MakeCall(Self.From,
                                     Self.From,
                                     Self.CallerMedia.LocalSessionDescription,
                                     Self.CallerMedia.MimeType,
                                     TIdSipRequest.DefaultMaxForwards);
  Self.Stack.Send(Self.Caller);
end;

procedure TUIForm.HangUpClick(Sender: TObject);
begin
  Self.Stack.Terminate(Self.Callee);
end;

procedure TUIForm.PartyPanelResize(Sender: TObject);
begin
  Self.CallerLog.Width := (Self.PartyPanel.Width - Self.PartySplitter.Width) div 2;
end;

procedure TUIForm.CallerSendsMediaClick(Sender: TObject);
begin
  Self.SendMedia(Self.CallerMedia);
end;

procedure TUIForm.CalleeSendsMediaClick(Sender: TObject);
begin
  Self.SendMedia(Self.CalleeMedia);
end;

end.
