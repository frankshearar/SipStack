unit TestFrameworkRtp;

interface

uses
  Classes, Contnrs, IdRTP, IdInterfacedObject, SyncObjs, SysUtils,
  TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
  end;

  TIdMockRTPPeer = class(TIdInterfacedObject,
                         IIdAbstractRTPPeer)
  private
    fLastRTP:   TIdRTPPacket;
    fProfile:   TIdRTPProfile;
    fRTCPCount: Cardinal;
    RTCPBuffer: TObjectList;

    function  GetLastRTCP: TIdRTCPPacket;
    function  GetRTCPCount: Cardinal;
    function  GetSecondLastRTCP: TIdRTCPPacket;
    procedure SetProfile(const Value: TIdRTPProfile);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure SendPacket(Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket);

    property LastRTCP:       TIdRTCPPacket read GetLastRTCP;
    property LastRTP:        TIdRTPPacket  read fLastRTP;
    property Profile:        TIdRTPProfile read fProfile write SetProfile;
    property RTCPCount:      Cardinal      read GetRTCPCount;
    property SecondLastRTCP: TIdRTCPPacket read GetSecondLastRTCP;
  end;

  TIdMockPayload = class(TIdRTPPayload)
  private
    fHasKnownLength: Boolean;
    fLength:         Cardinal;
  public
    function  HasKnownLength: Boolean; override;
    function  Length: Cardinal; override;
    procedure SetHasKnownLength(const Yes: Boolean);
    procedure SetLength(const Length: Cardinal);
  end;

  TIdMockProfile = class(TIdAudioVisualProfile)
  private
    fAllowExtension: Boolean;
  public
    function  AllowsHeaderExtensions: Boolean; override;
    procedure SetAllowExtension(const Allow: Boolean);
  end;

implementation

//******************************************************************************
//* TTestRTP                                                                   *
//******************************************************************************
//* TTestRTP Public methods ****************************************************

procedure TTestRTP.CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
var
  I: Integer;
begin
  CheckEquals(Expected.Version,            Received.Version,              'Version');
  CheckEquals(Expected.HasPadding,         Received.HasPadding,           'HasPadding');
  CheckEquals(Expected.CsrcCount,          Received.CsrcCount,            'CSRC count');
  CheckEquals(Expected.IsMarker,           Received.IsMarker,             'IsMarker');
  CheckEquals(Expected.PayloadType,        Received.PayloadType,          'PayloadType');
  CheckEquals(Expected.SequenceNo,         Received.SequenceNo,           'SequenceNo');
  CheckEquals(Integer(Expected.Timestamp), Integer(Received.Timestamp),   'Timestamp');
  CheckEquals(Integer(Expected.SyncSrcID), Integer(Received.SyncSrcID),   'SSRC ID');

  for I := 0 to Expected.CsrcCount - 1 do
    CheckEquals(Integer(Expected.CsrcIDs[I]),
                Integer(Received.CsrcIDs[I]),
                IntToStr(I) + 'th CSRC ID');
end;

//******************************************************************************
//* TIdMockRTPPeer                                                             *
//******************************************************************************
//* TIdMockRTPPeer Public methods **********************************************

constructor TIdMockRTPPeer.Create;
begin
  inherited Create;

  Self.fRTCPCount := 0;
  Self.RTCPBuffer := TObjectList.Create(true);
end;

destructor TIdMockRTPPeer.Destroy;
begin
  Self.RTCPBuffer.Free;
  Self.LastRTP.Free;

  inherited Destroy;
end;

procedure TIdMockRTPPeer.SendPacket(Host: String;
                                    Port: Cardinal;
                                    Packet: TIdRTPBasePacket);
begin
  if Packet.IsRTP then begin
    if not Assigned(Self.Profile) then
      raise Exception.Create('You didn''t set TIdMockRTPPeer.Profile');

    Self.fLastRTP.Assign(Packet)
  end
  else if Packet.IsRTCP then begin
    Self.RTCPBuffer.Add(Packet.Clone);
    Inc(Self.fRTCPCount);
  end;
end;

//* TIdMockRTPPeer Private methods *********************************************

function TIdMockRTPPeer.GetLastRTCP: TIdRTCPPacket;
begin
  Result := Self.RTCPBuffer[Self.RTCPBuffer.Count - 1] as TIdRTCPPacket;
end;

function TIdMockRTPPeer.GetRTCPCount: Cardinal;
begin
  Result := Self.RTCPBuffer.Count;
end;

function TIdMockRTPPeer.GetSecondLastRTCP: TIdRTCPPacket;
begin
  if (Self.RTCPCount < 2) then
    Result := nil
  else
    Result := Self.RTCPBuffer[Self.RTCPCount - 2] as TIdRTCPPacket;
end;

procedure TIdMockRTPPeer.SetProfile(const Value: TIdRTPProfile);
begin
  Self.fLastRTP := TIdRTPPacket.Create(Value);
  Self.fProfile := Value;
end;

//******************************************************************************
//* TIdMockPayload                                                             *
//******************************************************************************
//* TIdMockPayload Public methods **********************************************

function TIdMockPayload.HasKnownLength: Boolean;
begin
  Result := fHasKnownLength;
end;

function TIdMockPayload.Length: Cardinal;
begin
  Result := fLength;
end;

procedure TIdMockPayload.SetHasKnownLength(const Yes: Boolean);
begin
  fHasKnownLength := Yes;
end;

procedure TIdMockPayload.SetLength(const Length: Cardinal);
begin
  fLength := Length;
end;

//******************************************************************************
//* TIdMockProfile                                                             *
//******************************************************************************
//* TIdMockProfile Public methods **********************************************

function TIdMockProfile.AllowsHeaderExtensions: Boolean;
begin
  Result := fAllowExtension;
end;

procedure TIdMockProfile.SetAllowExtension(const Allow: Boolean);
begin
  fAllowExtension := Allow;
end;

end.
