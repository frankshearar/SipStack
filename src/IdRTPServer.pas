unit IdRTPServer;

interface

uses
  Classes, IdRTP, IdSocketHandle, IdUDPServer;

type
  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

  TIdRTPServer = class(TIdUDPServer)
  private
    FProfile:   TIdRTPProfile;
    FOnRTPRead: TIdRTPReadEvent;

    procedure DoOnRTPRead(APacket: TIdRTPPacket; ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property Profile: TIdRTPProfile read FProfile;
  published
    property OnRTPRead: TIdRTPReadEvent read FOnRTPRead write FOnRTPRead;
  end;

implementation

uses
  IdSipConsts, Math;

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FProfile := TIdAudioVisualProfile.Create;

  Self.ThreadedEvent := true;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;

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
    Pkt.ReadPayload(AData);
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
