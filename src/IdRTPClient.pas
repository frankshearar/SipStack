unit IdRTPClient;

interface

uses
  Classes, IdUDPClient, IdRTP;

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
    procedure Send(Packet: TIdRTPPayload); overload;

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
  S:      TStringStream;
begin
  S := TStringStream.Create(Data);
  try
    Self.Send(S, PayloadType);
  finally
    S.Free;
  end;
end;

procedure TIdRTPClient.Send(Data: TStream; PayloadType: TIdRTPPayloadType);
const
  BufLen = 1000;
var
  Buf:       array[1..BufLen] of Char;
  BytesRead: Cardinal;
  Packet:    TIdRTPPacket;
  S:         TStringStream;
begin
  FillChar(Buf, Length(Buf), 0);
  repeat
    Packet := TIdRTPPacket.Create(Self.Profile);
    try
      BytesRead := Data.Read(Buf, BufLen);

      if Self.FirstPacketSent then begin
        Self.SequenceNo  := Packet.SequenceNo;
        Packet.Timestamp := $DEADBEEF;
      end
      else begin
        Packet.SequenceNo := Self.NextSequenceNo;
        Packet.Timestamp  := $DEADBEEF;
      end;

      Packet.PayloadType := PayloadType;

      Packet.ReadPayload(Copy(Buf, 1, BytesRead));

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
  until (BytesRead < BufLen);
end;

procedure TIdRTPClient.Send(Packet: TIdRTPPayload);
begin
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
