unit TestFrameworkRtp;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdRTP, IdSocketHandle, SyncObjs,
  SysUtils, TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
  end;

  TIdMockRTPPeer = class(TIdInterfacedObject,
                         IIdAbstractRTPPeer)
  private
    ListenerLock: TCriticalSection;
    Listeners:    TList;
    fLastRTP:     TIdRTPPacket;
    fProfile:     TIdRTPProfile;
    fRTCPCount:   Cardinal;
    RTCPBuffer:   TObjectList;

    function  GetLastRTCP: TIdRTCPPacket;
    function  GetRTCPCount: Cardinal;
    function  GetSecondLastRTCP: TIdRTCPPacket;
    procedure SetProfile(Value: TIdRTPProfile);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdRTPListener);
    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdSocketHandle);
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdSocketHandle);
    procedure RemoveListener(const Listener: IIdRTPListener);
    procedure SendPacket(const Host: String;
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
    procedure SetLength(Length: Cardinal);
  end;

  TIdMockProfile = class(TIdAudioVisualProfile)
  private
    fAllowExtension: Boolean;
  public
    function  AllowsHeaderExtensions: Boolean; override;
    procedure SetAllowExtension(Allow: Boolean);
  end;

  TIdRTPTestRTPDataListener = class(TIdInterfacedObject,
                                IIdRTPDataListener)
  private
    fNewData: Boolean;
  public
    constructor Create;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);

    property NewData: Boolean read fNewData;
  end;

  TIdRTPTestRTPListener = class(TIdInterfacedObject,
                                IIdRTPListener)
  private
    fReceivedRTCP: Boolean;
    fReceivedRTP:  Boolean;
  public
    constructor Create;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdSocketHandle);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdSocketHandle);

    property ReceivedRTCP: Boolean read fReceivedRTCP;
    property ReceivedRTP:  Boolean read fReceivedRTP;
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

  Self.ListenerLock := TCriticalSection.Create;
  Self.Listeners    := TList.Create;

  Self.fRTCPCount := 0;
  Self.RTCPBuffer := TObjectList.Create(true);
end;

destructor TIdMockRTPPeer.Destroy;
begin
  Self.RTCPBuffer.Free;
  Self.LastRTP.Free;
  Self.Listeners.Free;
  Self.ListenerLock.Free;

  inherited Destroy;
end;

procedure TIdMockRTPPeer.AddListener(const Listener: IIdRTPListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Add(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                               Binding: TIdSocketHandle);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdRTPListener(Self.Listeners[I]).OnRTCP(Packet, Binding);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                              Binding: TIdSocketHandle);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdRTPListener(Self.Listeners[I]).OnRTP(Packet, Binding);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdMockRTPPeer.RemoveListener(const Listener: IIdRTPListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Remove(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdMockRTPPeer.SendPacket(const Host: String;
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

procedure TIdMockRTPPeer.SetProfile(Value: TIdRTPProfile);
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

procedure TIdMockPayload.SetLength(Length: Cardinal);
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

procedure TIdMockProfile.SetAllowExtension(Allow: Boolean);
begin
  fAllowExtension := Allow;
end;

//******************************************************************************
//* TIdRTPTestRTPDataListener                                                  *
//******************************************************************************
//* TIdRTPTestRTPDataListener Public methods ***********************************

constructor TIdRTPTestRTPDataListener.Create;
begin
  inherited Create;

  fNewData := false;
end;

procedure TIdRTPTestRTPDataListener.OnNewData(Data: TIdRTPPayload;
                                              Binding: TIdSocketHandle);
begin
  fNewData := true;
end;

//******************************************************************************
//* TIdRTPTestRTPListener                                                      *
//******************************************************************************
//* TIdRTPTestRTPListener Public methods ***************************************

constructor TIdRTPTestRTPListener.Create;
begin
  inherited Create;

  fReceivedRTCP := false;
  fReceivedRTP  := false;
end;

procedure TIdRTPTestRTPListener.OnRTCP(Packet: TIdRTCPPacket;
                                       Binding: TIdSocketHandle);
begin
  fReceivedRTCP := true;
end;

procedure TIdRTPTestRTPListener.OnRTP(Packet: TIdRTPPacket;
                                      Binding: TIdSocketHandle);
begin
  fReceivedRTP := true;
end;

end.
