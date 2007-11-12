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
  Classes, Contnrs, IdConnectionBindings, IdInterfacedObject, IdNotification,
  IdObservable, IdRTP, IdSipCore, IdTimerQueue, SyncObjs, SysUtils,
  TestFramework, TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
  end;

  TIdMockRTPPeer = class(TIdBaseRTPAbstractPeer)
  private
    fActive:               Boolean;
    fAddress:              String;
    fDefaultPort:          Integer;
    fLastPacketHostTarget: String;
    fLastPacketPortTarget: Cardinal;
    fRTCPCount:            Cardinal;
    fRTCPPort:             Cardinal;
    fRTPCount:             Cardinal;
    fRTPPort:              Cardinal;
    RTCPBuffer:            TObjectList;
    RTPBuffer:             TObjectList;

    function  GetLastRTCP: TIdRTCPPacket;
    function  GetLastRTP: TIdRTPPacket;
    function  GetRTCPCount: Cardinal;
    function  GetRTPCount: Cardinal;
    function  GetSecondLastRTCP: TIdRTCPPacket;
  protected
    function  GetActive: Boolean; override;
    function  GetAddress: String; override;
    function  GetDefaultPort: Integer; override;
    function  GetRTCPPort: Cardinal; override;
    function  GetRTPPort: Cardinal; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetAddress(Value: String); override;
    procedure SetDefaultPort(Value: Integer); override;
    procedure SetRTCPPort(Value: Cardinal); override;
    procedure SetRTPPort(Value: Cardinal); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdConnectionBindings); override;
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdConnectionBindings); override;
    procedure NotifyListenersOfSentRTCP(Packet: TIdRTCPPacket;
                                        Binding: TIdConnectionBindings); override;
    procedure NotifyListenersOfSentRTP(Packet: TIdRTPPacket;
                                       Binding: TIdConnectionBindings); override;
    procedure ReceivePacket(Packet: TIdRTPBasePacket;
                            Binding: TIdConnectionBindings); override;
    procedure SendPacket(const Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;

    property LastPacketHostTarget: String        read fLastPacketHostTarget;
    property LastPacketPortTarget: Cardinal      read fLastPacketPortTarget;
    property LastRTCP:             TIdRTCPPacket read GetLastRTCP;
    property LastRTP:              TIdRTPPacket  read GetLastRTP;
    property RTCPCount:            Cardinal      read GetRTCPCount;
    property RTPCount:             Cardinal      read GetRTPCount;
    property SecondLastRTCP:       TIdRTCPPacket read GetSecondLastRTCP;
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
    fBindingParam: TIdConnectionBindings;
    fDataParam:    TIdRTPPayload;
    fNewData:      Boolean;
  public
    constructor Create; override;

    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);

    property BindingParam: TIdConnectionBindings read fBindingParam;
    property DataParam:    TIdRTPPayload read fDataParam;
    property NewData:      Boolean       read fNewData;
  end;

  TIdRTPTestRTPListener = class(TIdInterfacedObject,
                                IIdRTPListener)
  private
    fBindingParam:    TIdConnectionBindings;
    fReceivedRTCP:    Boolean;
    fReceivedRTP:     Boolean;
    fRTCPPacketParam: TIdRTCPPacket;
    fRTPPacketParam:  TIdRTPPacket;
  public
    constructor Create; override;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);

    property BindingParam:    TIdConnectionBindings   read fBindingParam;
    property ReceivedRTCP:    Boolean         read fReceivedRTCP;
    property ReceivedRTP:     Boolean         read fReceivedRTP;
    property RTCPPacketParam: TIdRTCPPacket   read fRTCPPacketParam;
    property RTPPacketParam:  TIdRTPPacket    read fRTPPacketParam;
  end;

  TIdRTPTestRTPSendListener = class(TIdInterfacedObject,
                                    IIdRTPSendListener)
  private
    fBindingParam:    TIdConnectionBindings;
    fSentRTCP:        Boolean;
    fSentRTP:         Boolean;
    fRTCPPacketParam: TIdRTCPPacket;
    fRTPPacketParam:  TIdRTPPacket;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure OnSendRTCP(Packet: TIdRTCPPacket;
                         Binding: TIdConnectionBindings);
    procedure OnSendRTP(Packet: TIdRTPPacket;
                        Binding: TIdConnectionBindings);

    property BindingParam:    TIdConnectionBindings  read fBindingParam;
    property SentRTCP:        Boolean        read fSentRTCP;
    property SentRTP:         Boolean        read fSentRTP;
    property RTCPPacketParam: TIdRTCPPacket  read fRTCPPacketParam;
    property RTPPacketParam:  TIdRTPPacket   read fRTPPacketParam;
  end;

  TestTTestRTP = class(TTestRTP)
  published
    procedure TestCheckUnicode;
  end;

implementation

uses
  IdException, IdSocketHandle;

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

//******************************************************************************
//* TIdMockRTPPeer                                                             *
//******************************************************************************
//* TIdMockRTPPeer Public methods **********************************************

constructor TIdMockRTPPeer.Create;
begin
  inherited Create;

  Self.fRTCPCount := 0;
  Self.RTCPBuffer := TObjectList.Create(true);
  Self.fRTPCount  := 0;
  Self.RTPBuffer  := TObjectList.Create(true);

  // Set these values to something that will look like an uninitialised value.
  Self.fLastPacketHostTarget := 'uninitialised LastPacketHostTarget';
  Self.fLastPacketPortTarget := 31337;
end;

destructor TIdMockRTPPeer.Destroy;
begin
  Self.RTPBuffer.Free;
  Self.RTCPBuffer.Free;

  inherited Destroy;
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                               Binding: TIdConnectionBindings);
begin
  inherited NotifyListenersOfRTCP(Packet, Binding);
end;

procedure TIdMockRTPPeer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                              Binding: TIdConnectionBindings);
begin
  inherited NotifyListenersOfRTP(Packet, Binding);
end;

procedure TIdMockRTPPeer.NotifyListenersOfSentRTCP(Packet: TIdRTCPPacket;
                                                   Binding: TIdConnectionBindings);
begin
  inherited NotifyListenersOfSentRTCP(Packet, Binding);
end;

procedure TIdMockRTPPeer.NotifyListenersOfSentRTP(Packet: TIdRTPPacket;
                                                  Binding: TIdConnectionBindings);
begin
  inherited NotifyListenersOfSentRTP(Packet, Binding);
end;

procedure TIdMockRTPPeer.ReceivePacket(Packet: TIdRTPBasePacket;
                                       Binding: TIdConnectionBindings);
var
  ReceivedPacket: TIdRTPBasePacket;
  S:              TStringStream;
begin
  if Self.Active then begin
    // This may look peculiar, but we're mimicking the behaviour of a
    // TIdRTPServer. See TIdRTPServer.DoOnUdpRead.
    S := TStringStream.Create('');
    try
      Packet.PrintOn(S);
      S.Seek(0, soFromBeginning);

      ReceivedPacket := Self.RemoteProfile.CreatePacket(S);
      try
        ReceivedPacket.ReadFrom(S);

        inherited ReceivePacket(ReceivedPacket, Binding);
      finally
        ReceivedPacket.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TIdMockRTPPeer.SendPacket(const Host: String;
                                    Port: Cardinal;
                                    Packet: TIdRTPBasePacket);
var
  Binding: TIdConnectionBindings;
begin
  if Packet.IsRTP then begin
    Self.RTPBuffer.Add(Packet.Copy);
    Inc(Self.fRTPCount);
  end
  else if Packet.IsRTCP then begin
    Self.RTCPBuffer.Add(Packet.Copy);
    Inc(Self.fRTCPCount);
  end;

  Self.fLastPacketHostTarget := Host;
  Self.fLastPacketPortTarget := Port;

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP  := Self.Address;
    Binding.PeerIP   := Host;
    Binding.PeerPort := Port;
    if Packet.IsRTP then begin
      Binding.LocalPort := Self.RTPPort;
      Self.NotifyListenersOfSentRTP(Packet as TIdRTPPacket, Binding);
    end
    else begin
      Binding.LocalPort := Self.RTCPPort;
      Self.NotifyListenersOfSentRTCP(Packet as TIdRTCPPacket, Binding);
    end;
  finally
    Binding.Free;
  end;
end;

//* TIdMockRTPPeer Protected methods *******************************************

function TIdMockRTPPeer.GetActive: Boolean;
begin
  Result := Self.fActive;
end;

function TIdMockRTPPeer.GetAddress: String;
begin
  Result := Self.fAddress;
end;

function TIdMockRTPPeer.GetDefaultPort: Integer;
begin
  Result := Self.fDefaultPort;
end;

function TIdMockRTPPeer.GetRTCPPort: Cardinal;
begin
  Result := Self.fRTCPPort;
end;

function TIdMockRTPPeer.GetRTPPort: Cardinal;
begin
  Result := Self.fRTPPort;
end;

procedure TIdMockRTPPeer.SetActive(Value: Boolean);
begin
  if Value then begin
    if TIdRTPPeerRegistry.ServerRunningOn(Self.Address, Self.RTPPort) then
      raise EIdSocketError.Create('');
    if TIdRTPPeerRegistry.ServerRunningOn(Self.Address, Self.RTCPPort) then
      raise EIdSocketError.Create('');
  end;
  
  Self.fActive := Value;
end;

procedure TIdMockRTPPeer.SetAddress(Value: String);
begin
  Self.fAddress := Value;
end;

procedure TIdMockRTPPeer.SetDefaultPort(Value: Integer);
begin
  Self.fDefaultPort := Value;
end;

procedure TIdMockRTPPeer.SetRTCPPort(Value: Cardinal);
begin
  Self.fRTCPPort := Value;
end;

procedure TIdMockRTPPeer.SetRTPPort(Value: Cardinal);
begin
  Self.fRTPPort := Value;
end;

//* TIdMockRTPPeer Private methods *********************************************

function TIdMockRTPPeer.GetLastRTCP: TIdRTCPPacket;
begin
  Result := Self.RTCPBuffer[Self.RTCPBuffer.Count - 1] as TIdRTCPPacket;
end;

function TIdMockRTPPeer.GetLastRTP: TIdRTPPacket;
begin
  Result := Self.RTPBuffer[Self.RTPBuffer.Count - 1] as TIdRTPPacket;
end;

function TIdMockRTPPeer.GetRTCPCount: Cardinal;
begin
  Result := Self.RTCPBuffer.Count;
end;

function TIdMockRTPPeer.GetRTPCount: Cardinal;
begin
  Result := Self.RTPBuffer.Count;
end;

function TIdMockRTPPeer.GetSecondLastRTCP: TIdRTCPPacket;
begin
  if (Self.RTCPCount < 2) then
    Result := nil
  else
    Result := Self.RTCPBuffer[Self.RTCPCount - 2] as TIdRTCPPacket;
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
                                              Binding: TIdConnectionBindings);
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
                                       Binding: TIdConnectionBindings);
begin
  Self.fBindingParam    := Binding;
  Self.fRTCPPacketParam := Packet;
  Self.fReceivedRTCP    := true;
end;

procedure TIdRTPTestRTPListener.OnRTP(Packet: TIdRTPPacket;
                                      Binding: TIdConnectionBindings);
begin
  Self.fBindingParam   := Binding;
  Self.fRTPPacketParam := Packet;
  Self.fReceivedRTP    := true;
end;

//******************************************************************************
//* TIdRTPTestRTPSendListener                                                  *
//******************************************************************************
//* TIdRTPTestRTPSendListener Public methods ***********************************

constructor TIdRTPTestRTPSendListener.Create;
begin
  inherited Create;

  Self.fBindingParam := TIdConnectionBindings.Create;

  Self.fSentRTCP := false;
  Self.fSentRTP  := false;
end;

destructor TIdRTPTestRTPSendListener.Destroy;
begin
  Self.fBindingParam.Free;

  inherited Destroy;
end;

procedure TIdRTPTestRTPSendListener.OnSendRTCP(Packet: TIdRTCPPacket;
                                               Binding: TIdConnectionBindings);
begin
  Self.fBindingParam.Assign(Binding);
  Self.fRTCPPacketParam := Packet;
  Self.fSentRTCP        := true;
end;

procedure TIdRTPTestRTPSendListener.OnSendRTP(Packet: TIdRTPPacket;
                                              Binding: TIdConnectionBindings);
begin
  Self.fBindingParam.Assign(Binding);
  Self.fRTPPacketParam := Packet;
  Self.fSentRTP        := true;
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
