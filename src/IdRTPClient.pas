unit IdRTPClient;

interface

uses
  Classes, IdUDPClient, IdRTPServer;

type
  TIdRTPClient = class(TIdUDPClient)
  private
    DefaultProfile:      TIdRTPProfile;
    FirstPacketSent:     Boolean;
    fMaximumPayloadSize: Cardinal;
    fProfile:            TIdRTPProfile;
    SequenceNo:          TIdRTPSequenceNo;

    function GetProfile: TIdRTPProfile;
    function NextSequenceNo: TIdRTPSequenceNo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  DefaultMaximumPayloadSize: Cardinal; virtual;
    procedure Send(Data: String; PayloadType: TIdRTPPayloadType); overload;
    procedure Send(Data: TStream; PayloadType: TIdRTPPayloadType); overload;

    property MaximumPayloadSize: Cardinal      read fMaximumPayloadSize write fMaximumPayloadSize;
    property Profile:            TIdRTPProfile read GetProfile write fProfile;
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdRTPClient                                                               *
//******************************************************************************
//* TIdRTPClient Public methods ************************************************

constructor TIdRTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FirstPacketSent := true;
  Self.DefaultProfile := TIdAudioVisualProfile.Create;

  Self.MaximumPayloadSize := Self.DefaultMaximumPayloadSize;
end;

destructor TIdRTPClient.Destroy;
begin
  Self.DefaultProfile.Free;

  inherited Destroy;
end;

function TIdRTPClient.DefaultMaximumPayloadSize: Cardinal;
begin
  Result := 1000;
end;

procedure TIdRTPClient.Send(Data: String; PayloadType: TIdRTPPayloadType);
var
  Buffer: String;
  Packet: TIdRTPPacket;
  S:      TStringStream;
  Size:   Cardinal;
begin
  Size := Self.MaximumPayloadSize;

  while (Data <> '') do begin
    Buffer := Copy(Data, 1, Size);
    Delete(Data, 1, Size);

    Packet := TIdRTPPacket.Create(Self.Profile);
    try
      if Self.FirstPacketSent then begin
        Self.SequenceNo  := Packet.SequenceNo;
        Packet.Timestamp := $DEADBEEF;
      end
      else begin
        Packet.SequenceNo := Self.NextSequenceNo;
        Packet.Timestamp  := $DEADBEEF;
      end;

      Packet.PayloadType := PayloadType;

      Packet.ReadPayload(Buffer);

      S := TStringStream.Create('');
      try
        Packet.PrintOn(S);

        Self.Send(S.DataString);
        Self.FirstPacketSent := false;
      finally
        S.Free;
      end;
    finally
      Packet.Free;
    end;
  end;
end;

procedure TIdRTPClient.Send(Data: TStream; PayloadType: TIdRTPPayloadType);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(Data, 0);
    Self.Send(S.DataString, PayloadType);
  finally
    S.Free;
  end;
end;

//* TIdRTPClient Private methods ***********************************************

function TIdRTPClient.GetProfile: TIdRTPProfile;
begin
  if Assigned(fProfile) then
    Result := fProfile
  else
    Result := Self.DefaultProfile
end;

function TIdRTPClient.NextSequenceNo: TIdRTPSequenceNo;
begin
  Inc(Self.SequenceNo);
  Result := Self.SequenceNo;
end;

end.
