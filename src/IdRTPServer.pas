unit IdRTPServer;

interface

uses
  Classes, Contnrs, IdRTP, IdSocketHandle, IdUDPServer;

type
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

  TIdRTPMemberEntry = class(TObject)
  private
    fControlAddress: String;
    fSourceAddress:  String;
    fSSRC:           Cardinal;
  public
    property ControlAddress: String   read fControlAddress write fControlAddress;
    property SourceAddress:  String   read fSourceAddress write fSourceAddress;
    property SSRC:           Cardinal read fSSRC write fSSRC;
  end;

  TIdRTPMembers = class(TObject)
  private
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

  TIdRTPServer = class(TIdUDPServer)
  private
    FControlPort: Cardinal;
    FMemberTable: TIdRTPMembers;
    FOnRTPRead:   TIdRTPReadEvent;
    FProfile:     TIdRTPProfile;

    procedure DoOnRTPRead(APacket: TIdRTPPacket; ABinding: TIdSocketHandle);

    property MemberTable: TIdRTPMembers read FMemberTable;
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property Profile: TIdRTPProfile read FProfile;
  published

    property ControlPort: Cardinal        read FControlPort write FControlPort;
    property OnRTPRead:   TIdRTPReadEvent read FOnRTPRead write FOnRTPRead;
  end;

implementation

uses
  Math;

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
  Pkt: TIdRTPPacket;
begin
  inherited DoUDPRead(AData, ABinding);

  Pkt := TIdRTPPacket.Create(Self.Profile);
  try
    Pkt.ReadFrom(AData);
    Self.DoOnRTPRead(Pkt, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

procedure TIdRTPServer.DoOnRTPRead(APacket: TIdRTPPacket;
                                   ABinding: TIdSocketHandle);
begin
  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, APacket, ABinding);
end;

end.
