{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkRtp;

interface

uses
  Classes, Contnrs, IdInterfacedObject, IdNotification, IdObservable, IdRTP, IdSipCore,
  SyncObjs, SysUtils, TestFramework, TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
    procedure CheckEqualsW(Expected,
                           Actual: WideString;
                           Msg: String);
    procedure CheckUnicode(Expected: WideString;
                           Actual: String;
                           Msg: String);
  end;

  TIdMockRTPPeer = class(TIdInterfacedObject,
                         IIdAbstractRTPPeer)
  private
    Listeners:    TIdNotificationList;
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
                                    Binding: TIdConnection);
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdConnection);
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
    fBindingParam: TIdConnection;
    fDataParam:    TIdRTPPayload;
    fNewData:      Boolean;
  public
    constructor Create;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnection);

    property BindingParam: TIdConnection read fBindingParam;
    property DataParam: TIdRTPPayload      read fDataParam;
    property NewData:   Boolean            read fNewData;
  end;

  TIdRTPTestRTPListener = class(TIdInterfacedObject,
                                IIdRTPListener)
  private
    fBindingParam:    TIdConnection;
    fReceivedRTCP:    Boolean;
    fReceivedRTP:     Boolean;
    fRTCPPacketParam: TIdRTCPPacket;
    fRTPPacketParam:  TIdRTPPacket;
  public
    constructor Create;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnection);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnection);

    property BindingParam:    TIdConnection   read fBindingParam;
    property ReceivedRTCP:    Boolean         read fReceivedRTCP;
    property ReceivedRTP:     Boolean         read fReceivedRTP;
    property RTCPPacketParam: TIdRTCPPacket   read fRTCPPacketParam;
    property RTPPacketParam:  TIdRTPPacket    read fRTPPacketParam;
  end;

  TestTTestRTP = class(TTestRTP)
  published
    procedure TestCheckUnicode;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('TestFrameworkRtp unit tests');
  Result.AddTest(TestTTestRTP.Suite);
end;

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

procedure TTestRTP.CheckEqualsW(Expected,
                                Actual: WideString;
                                Msg: String);
begin
  if (Expected <> Actual) then
    FailNotEquals(Expected, Actual, Msg, CallerAddr);
end;

procedure TTestRTP.CheckUnicode(Expected: WideString;
                                Actual: String;
                                Msg: String);
var
  ActualI:   Integer;
  ExpectedI: Integer;
  W:         WideChar;
begin
  // Check that Actual contains the same data, byte-for-byte, as Expected.

  ActualI   := 1;
  ExpectedI := 1;
  while ActualI <= Length(Actual) - 1 do begin
    W := WideChar((Ord(Actual[ActualI]) shl 8) + Ord(Actual[ActualI + 1]));
    Check(Expected[ExpectedI] = W,
          Msg + ': character ' + IntToStr(ExpectedI)
              + ' ($' + IntToHex(Ord(W), 4) + ') differs');
    Inc(ActualI, SizeOf(WideChar));
    Inc(ExpectedI);
  end;

  CheckEquals(Length(Expected),
              Length(Actual) div SizeOf(WideChar),
              Msg + ': differing lengths');
end;

//******************************************************************************
//* TIdMockRTPPeer                                                             *
//******************************************************************************
//* TIdMockRTPPeer Public methods **********************************************

constructor TIdMockRTPPeer.Create;
begin
  inherited Create;

  Self.Listeners := TIdNotificationList.Create;

  Self.fRTCPCount := 0;
  Self.RTCPBuffer := TObjectList.Create(true);
end;

destructor TIdMockRTPPeer.Destroy;
begin
  Self.RTCPBuffer.Free;
  Self.LastRTP.Free;
  Self.Listeners.Free;

  inherited Destroy;
end;

procedure TIdMockRTPPeer.AddListener(const Listener: IIdRTPListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                               Binding: TIdConnection);
var
  Notification: TIdRTPListenerReceiveRTCPMethod;
begin
  Notification := TIdRTPListenerReceiveRTCPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                              Binding: TIdConnection);
var
  Notification: TIdRTPListenerReceiveRTPMethod;
begin
  Notification := TIdRTPListenerReceiveRTPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdMockRTPPeer.RemoveListener(const Listener: IIdRTPListener);
begin
  Self.Listeners.RemoveListener(Listener);
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
                                              Binding: TIdConnection);
begin
  Self.fBindingParam := Binding;
  Self.fDataParam    := Data;
  Self.fNewData      := true;
end;

//******************************************************************************
//* TIdRTPTestRTPListener                                                      *
//******************************************************************************
//* TIdRTPTestRTPListener Public methods ***************************************

constructor TIdRTPTestRTPListener.Create;
begin
  inherited Create;

  Self.fReceivedRTCP := false;
  Self.fReceivedRTP  := false;
end;

procedure TIdRTPTestRTPListener.OnRTCP(Packet: TIdRTCPPacket;
                                       Binding: TIdConnection);
begin
  Self.fBindingParam    := Binding;
  Self.fRTCPPacketParam := Packet;
  Self.fReceivedRTCP    := true;
end;

procedure TIdRTPTestRTPListener.OnRTP(Packet: TIdRTPPacket;
                                      Binding: TIdConnection);
begin
  Self.fBindingParam   := Binding;
  Self.fRTPPacketParam := Packet;
  Self.fReceivedRTP    := true;
end;

//******************************************************************************
//* TestTTestRTP                                                               *
//******************************************************************************
//* TestTTestRTP Published methods *********************************************

procedure TestTTestRTP.TestCheckUnicode;
var
  S:                String;
  ShouldHaveFailed: Boolean;
  W:                WideString;
begin
  W := '';
  S := '';
  CheckUnicode(W, S, 'Empty strings');

  W := 'Cthulhu';
  S := 'Cthulhu';
  try
    CheckUnicode(W, S, '''Cthulhu''');
    ShouldHaveFailed := true;
  except
    on ETestFailure do
      ShouldHaveFailed := false;
  end;
  if ShouldHaveFailed then
    Fail('Failed to bail out on ''Cthulhu''');

  W := 'Cthulhu';
  S := #0'C'#0't'#0'h'#0'u'#0'l'#0'h'#0'u';
  CheckUnicode(W, S, '''Cthulhu''');
end;

initialization
  RegisterTest('RTP test framework', Suite);
end.
