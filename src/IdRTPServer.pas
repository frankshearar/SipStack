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

  TIdRTPMembers = class(TObject)
  private
    List: TObjectList;
    function Find(const SSRC: Cardinal): TIdRTPMember;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(const SSRC: Cardinal): TIdRTPMember;
    function  Contains(Source: Cardinal): Boolean;
    function  Count: Integer;
    function  Member(const SSRC: Cardinal): TIdRTPMember;
    procedure Remove(const SSRC: Cardinal);
  end;

  TIdRTPServer = class(TIdUDPServer)
  private
    FControlPort: Cardinal;
    FMemberTable: TIdRTPMembers;
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

    property Profile: TIdRTPProfile read FProfile;
  published

    property ControlPort: Cardinal         read FControlPort write FControlPort;
    property MemberTable: TIdRTPMembers    read FMemberTable;
    property OnRTCPRead:  TIdRTCPReadEvent read FOnRTCPRead write FOnRTCPRead;
    property OnRTPRead:   TIdRTPReadEvent  read FOnRTPRead write FOnRTPRead;
  end;

implementation

uses
  Math;

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
//* TIdRTPMembers                                                              *
//******************************************************************************
//* TIdRTPMembers Public methods ***********************************************

constructor TIdRTPMembers.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdRTPMembers.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdRTPMembers.Add(const SSRC: Cardinal): TIdRTPMember;
begin
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

function TIdRTPMembers.Contains(Source: Cardinal): Boolean;
begin
  Result := Assigned(Self.Find(Source));
end;

function TIdRTPMembers.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdRTPMembers.Member(const SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Find(SSRC);
end;

procedure TIdRTPMembers.Remove(const SSRC: Cardinal);
begin
  Self.List.Remove(Self.Find(SSRC));
end;

//* TIdRTPMembers Private methods **********************************************

function TIdRTPMembers.Find(const SSRC: Cardinal): TIdRTPMember;
var
  I: Integer;
begin
  Result := nil;
  I := 0;

  while (I < Self.Count) and not Assigned(Result) do
    if ((Self.List[I] as TIdRTPMember).SSRC = SSRC) then
      Result := Self.List[I] as TIdRTPMember
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

  Self.FMemberTable := TIdRTPMembers.Create;
  Self.FProfile     := TIdAudioVisualProfile.Create;

  Self.ThreadedEvent := true;

  Self.DefaultPort := 8000;
  Self.ControlPort := 8001;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;
  Self.MemberTable.Free;

  inherited Destroy;
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

    if not Self.MemberTable.Contains(Pkt.SyncSrcID) then begin
      NewMember := Self.MemberTable.Add(Pkt.SyncSrcID);

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
