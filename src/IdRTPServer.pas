unit IdRTPServer;

interface

uses
  Classes, Contnrs, IdRTP, IdSocketHandle, IdUDPServer;

type
  TIdRTCPReadEvent = procedure(Sender: TObject;
                               APacket: TIdRTCPPacket;
                               ABinding: TIdSocketHandle) of object;
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

  TIdRTPMember = class(TObject)
  private
    fControlAddress: String;
    fControlPort:    Cardinal;
    fSourceAddress:  String;
    fSourcePort:     Cardinal;
    fSSRC:           Cardinal;
  public
    constructor Create;

    property ControlAddress: String   read fControlAddress write fControlAddress;
    property ControlPort:    Cardinal read fControlPort write fControlPort;
    property SourceAddress:  String   read fSourceAddress write fSourceAddress;
    property SourcePort:     Cardinal read fSourcePort write fSourcePort;
    property SSRC:           Cardinal read fSSRC write fSSRC;
  end;


  TIdAbstractRTPPeer = class(TIdUDPServer)
  public
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); virtual; abstract;
  end;


  // I provide a self-contained SSRC space.
  TIdRTPSession = class(TObject)
  private
    Agent:       TIdAbstractRTPPeer;
    CurrentTime: Cardinal;
    List:        TObjectList;
    NoDataSent:  Boolean;
    Profile:     TIdRTPProfile;
    SequenceNo:  TIdRTPSequenceNo;

    function Find(SSRC: Cardinal): TIdRTPMember;
  public
    constructor Create(Agent: TIdAbstractRTPPeer;
                       Profile: TIdRTPProfile);
    destructor  Destroy; override;

    function  Add(SSRC: Cardinal): TIdRTPMember;
    function  Contains(SSRC: Cardinal): Boolean;
    function  Count: Integer;
    function  MemberAt(Index: Cardinal): TIdRTPMember;
    function  Member(SSRC: Cardinal): TIdRTPMember;
    procedure Remove(SSRC: Cardinal);

    procedure PrepareControl(SR: TIdRTCPSenderReportPacket);
    procedure PrepareData(Packet: TIdRTPPacket);
    procedure SendControl(Packet: TIdRTCPPacket);
    procedure SendData(Packet: TIdRTPPacket);

    procedure Initialize;
    function  NextSequenceNo: TIdRTPSequenceNo;
  end;

  // While I look like a Server, I really represent a peer in an RTP session.
  // You may use me not only to receive RTP/RTCP data, but also to send data.
  //
  // As an implementation detail, the SSRC with value zero has a special
  // meaning. It represents the null SSRC. Equivalently, if CurrentSSRC has a
  // value of zero then I have not joined an RTP session.
  TIdRTPServer = class(TIdAbstractRTPPeer)
  private
    CurrentSSRC:  Cardinal;
    FControlPort: Cardinal;
    FSession:     TIdRTPSession;
    FOnRTCPRead:  TIdRTCPReadEvent;
    FOnRTPRead:   TIdRTPReadEvent;
    FProfile:     TIdRTPProfile;

    procedure DoOnRTCPRead(APacket: TIdRTCPPacket; ABinding: TIdSocketHandle);
    procedure DoOnRTPRead(APacket: TIdRTPPacket; ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  AcceptableSSRC(SSRC: Cardinal): Boolean;
    procedure JoinSession(Host: String; Port: Cardinal);
    procedure LeaveSession;
    function  NewSSRC: Cardinal;
    procedure SendData(Data: TIdRTPPayload);
    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;
    procedure SendSenderReport;

    property Profile: TIdRTPProfile read FProfile;
  published
    property ControlPort: Cardinal         read FControlPort write FControlPort;
    property Session:     TIdRTPSession    read FSession;
    property OnRTCPRead:  TIdRTCPReadEvent read FOnRTCPRead write FOnRTCPRead;
    property OnRTPRead:   TIdRTPReadEvent  read FOnRTPRead write FOnRTPRead;
  end;

implementation

uses
  IdGlobal, IdHash, IdHashMessageDigest, IdRTPTimerQueue, IdRandom, Math,
  SysUtils;

const
  ZeroSSRC = 0;

//******************************************************************************
//* TIdRTPMember                                                               *
//******************************************************************************
//* TIdRTPMember Public methods ************************************************

constructor TIdRTPMember.Create;
begin
  inherited Create;

  Self.ControlAddress := '';
  Self.ControlPort    := 0;
  Self.SourceAddress  := '';
  Self.SourcePort     := 0;
end;

//******************************************************************************
//* TIdRTPSession                                                              *
//******************************************************************************
//* TIdRTPSession Public methods ***********************************************

constructor TIdRTPSession.Create(Agent: TIdAbstractRTPPeer;
                                 Profile: TIdRTPProfile);
begin
  inherited Create;

  Self.Agent      := Agent;
  Self.List       := TObjectList.Create(true);
  Self.NoDataSent := true;
  Self.Profile    := Profile;
end;

destructor TIdRTPSession.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdRTPSession.Add(SSRC: Cardinal): TIdRTPMember;
begin
  if Self.Contains(SSRC) then begin
    Result := Self.Find(SSRC);
    Exit;
  end;

  Result := TIdRTPMember.Create;
  try
    Self.List.Add(Result);

    Result.ControlAddress := '';
    Result.ControlPort    := 0;
    Result.SourceAddress  := '';
    Result.SourcePort     := 0;
    Result.SSRC           := SSRC;
  except
    Self.List.Remove(Result);

    raise;
  end;
end;

function TIdRTPSession.Contains(SSRC: Cardinal): Boolean;
begin
  Result := Assigned(Self.Find(SSRC));
end;

function TIdRTPSession.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdRTPSession.Initialize;
begin
  SequenceNo := TIdRandomNumber.Next(High(SequenceNo));
end;

function TIdRTPSession.NextSequenceNo: TIdRTPSequenceNo;
begin
  Result := SequenceNo;
  SequenceNo := AddModulo(SequenceNo, 1, High(SequenceNo));
end;

function TIdRTPSession.MemberAt(Index: Cardinal): TIdRTPMember;
begin
  Result := Self.List[Index] as TIdRTPMember;
end;

function TIdRTPSession.Member(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Find(SSRC);
end;

procedure TIdRTPSession.Remove(SSRC: Cardinal);
begin
  Self.List.Remove(Self.Find(SSRC));
end;

procedure TIdRTPSession.PrepareControl(SR: TIdRTCPSenderReportPacket);
begin
  SR.NTPTimestamp := NowAsNTP;
end;

procedure TIdRTPSession.PrepareData(Packet: TIdRTPPacket);
begin
  if Self.NoDataSent then begin
    Self.SequenceNo  := TIdRandomNumber.Next(High(Self.SequenceNo));
    Self.CurrentTime := TIdRandomNumber.Next;
    Self.NoDataSent := false;
  end;

  Inc(Self.CurrentTime, Packet.Payload.NumberOfSamples);

  Packet.Timestamp  := Self.CurrentTime;
  Packet.SequenceNo := Self.NextSequenceNo;
end;

procedure TIdRTPSession.SendControl(Packet: TIdRTCPPacket);
var
  I: Integer;
begin
  Self.PrepareControl(Packet as TIdRTCPSenderReportPacket);

  for I := 0 to Self.Count - 1 do
    Agent.SendPacket(Self.MemberAt(I).SourceAddress,
                     Self.MemberAt(I).SourcePort,
                     Packet);
end;

procedure TIdRTPSession.SendData(Packet: TIdRTPPacket);
var
  I: Integer;
begin
  Self.PrepareData(Packet);

  for I := 0 to Self.Count - 1 do
    Agent.SendPacket(Self.MemberAt(I).SourceAddress,
                     Self.MemberAt(I).SourcePort,
                     Packet);
end;

//* TIdRTPSession Private methods **********************************************

function TIdRTPSession.Find(SSRC: Cardinal): TIdRTPMember;
var
  I: Integer;
begin
  Result := nil;
  I := 0;

  while (I < Self.Count) and not Assigned(Result) do
    if (Self.MemberAt(I).SSRC = SSRC) then
      Result := Self.MemberAt(I)
    else
      Inc(I);
end;  

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FProfile     := TIdAudioVisualProfile.Create;
  Self.FSession := TIdRTPSession.Create(Self, Self.Profile);

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;
  Self.Session.Free;

  inherited Destroy;
end;

function TIdRTPServer.AcceptableSSRC(SSRC: Cardinal): Boolean;
begin
  Result := (SSRC <> 0) and not Self.Session.Contains(SSRC);
end;

procedure TIdRTPServer.JoinSession(Host: String; Port: Cardinal);
var
  Pkt: TIdRTPPacket;
  S:   TStringStream;
begin
  Pkt := TIdRTPPacket.Create(Self.Profile);
  try
    Self.Session.PrepareData(Pkt);
    S := TStringStream.Create('');
    try
      Pkt.PrintOn(S);
      Self.Send(Host, Port, S.DataString);
    finally
      S.Free;
    end;
  finally
    Pkt.Free;
  end;
end;

procedure TIdRTPServer.LeaveSession;
var
  Bye: TIdRTCPByePacket;
begin
  Bye := TIdRTCPByePacket.Create;
  try
    Self.Session.SendControl(Bye);
  finally
    Bye.Free;
  end;
end;

function TIdRTPServer.NewSSRC: Cardinal;
var
  Hash:   T4x4LongWordRecord;
  Hasher: TIdHash128;
  I:      Integer;
begin
  Result := 0;
  // This implementation's largely stolen from Appendix A.6 of RFC 3550.
  Hasher := TIdHashMessageDigest5.Create;
  try
    while not Self.AcceptableSSRC(Result) do begin
      // TODO: We should add more stuff here. RFC 3550's uses: pid, uid, gid and
      // hostid (but hostid is deprecated according to FreeBSD's gethostid(3)
      // manpage).
      Hash := Hasher.HashValue(DateTimeToStr(Now)
                             + IndyGetHostName
                             + IntToHex(CurrentProcessId, 8)
                             + IntToHex(TIdRandomNumber.Next, 8));

      Result := 0;
      for I := Low(Hash) to High(Hash) do
        Result := Result xor Hash[I];
    end;
  finally
    Hasher.Free;
  end;
end;

procedure TIdRTPServer.SendData(Data: TIdRTPPayload);
var
  RTP: TIdRTPPacket;
  S:   TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    RTP := TIdRTPPacket.Create(Self.Session.Profile);
    try
      Data.PrintOn(S);
      RTP.PayloadType := Self.Session.Profile.PayloadTypeFor(Data.Encoding);
      RTP.ReadPayload(S);

      S.Seek(soFromBeginning, 0);
      RTP.PrintOn(S);
      Self.Session.SendData(RTP);
    finally
    end;
  finally
    S.Free;
  end;
end;

procedure TIdRTPServer.SendPacket(Host: String;
                                  Port: Cardinal;
                                  Packet: TIdRTPBasePacket);
var
  S: TStringStream;
begin
  if (Self.CurrentSSRC = ZeroSSRC) then begin
    Self.CurrentSSRC  := Self.NewSSRC;
    Self.Session.Add(Self.CurrentSSRC);
  end;

//  Self.CurrentSeqNo := AddModulo(Self.CurrentSeqNo, 1, High(Self.CurrentSeqNo));
  Packet.SyncSrcID := Self.CurrentSSRC;

  S := TStringStream.Create('');
  try
    Packet.PrintOn(S);
    Self.Send(Host, Port, S.DataString);
  finally
    S.Free;
  end;
end;

procedure TIdRTPServer.SendSenderReport;
var
  SR: TIdRTCPSenderReportPacket;
begin
  SR := TIdRTCPSenderReportPacket.Create;
  try
    Self.Session.SendControl(SR);
  finally
    SR.Free;
  end;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  Pkt:       TIdRTPBasePacket;
  NewMember: TIdRTPMember;
begin
  inherited DoUDPRead(AData, ABinding);

  Pkt := TIdRTPBasePacket.CreateFrom(AData, Self.Profile);
  try
    Pkt.ReadFrom(AData);

    if not Self.Session.Contains(Pkt.SyncSrcID) then begin
      NewMember := Self.Session.Add(Pkt.SyncSrcID);

      if Pkt.IsRTP then begin
        NewMember.SourceAddress := ABinding.PeerIP;
        NewMember.SourcePort    := ABinding.PeerPort;
      end
      else begin
        NewMember.ControlAddress := ABinding.PeerIP;
        NewMember.ControlPort    := ABinding.PeerPort;
      end;
    end;

    if Pkt.IsRTP then
      Self.DoOnRTPRead(Pkt as TIdRTPPacket, ABinding)
    else
      Self.DoOnRTCPRead(Pkt as TIdRTCPPacket, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

procedure TIdRTPServer.DoOnRTCPRead(APacket: TIdRTCPPacket;
                                   ABinding: TIdSocketHandle);
begin
  if Assigned(Self.OnRTCPRead) then
    Self.OnRTCPRead(Self, APacket, ABinding);
end;

procedure TIdRTPServer.DoOnRTPRead(APacket: TIdRTPPacket;
                                   ABinding: TIdSocketHandle);
begin
  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, APacket, ABinding);
end;

end.
