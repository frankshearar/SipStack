unit TestIdRTPServer;

interface

uses
  Classes, IdRTP, IdRTPClient, IdRTPServer, IdSocketHandle, IdUDPServer,
  TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPServer = class(TTestRTP)
  private
    Client: TIdUDPServer;
    Packet: TIdRTPPacket;
    Server: TIdRTPServer;
    procedure CheckReceivePacket(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
    procedure CheckOnRTCPRead(Sender: TObject;
                             APacket: TIdRTCPPacket;
                             ABinding: TIdSocketHandle);
    procedure CheckOnRTPRead(Sender: TObject;
                             APacket: TIdRTPPacket;
                             ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOnRTCPRead;
    procedure TestOnRTPRead;
    procedure TestOnUDPRead;
  end;

  TestTIdRTPMembers = class(TTestCase)
  private
    Members: TIdRTPMembers;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestContains;
    procedure TestRemove;
  end;

  TestT140 = class(TThreadingTestCase)
  private
    Client:   TIdRTPClient;
    Msg:      String;
    Server:   TIdRTPServer;
    T140PT:   TIdRTPPayloadType;

    procedure StoreT140Data(Sender: TObject;
                            APacket: TIdRTPPacket;
                            ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTransmission;
  end;

const
  DefaultTimeout = 5000; // ms

implementation

uses
  SyncObjs, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');
  Result.AddTest(TestTIdRTPServer.Suite);
  Result.AddTest(TestTIdRTPMembers.Suite);
  Result.AddTest(TestT140.Suite);
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
var
  Binding:    TIdSocketHandle;
  NoEncoding: TIdRTPEncoding;
  PT:         TIdRTPPayloadType;
begin
  inherited SetUp;

  PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Binding := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5004;

  NoEncoding := TIdRTPEncoding.Create('No encoding', 0);
  try
    Self.Server.Profile.AddEncoding(NoEncoding, PT);
  finally
    NoEncoding.Free;
  end;

  Self.Packet := TIdRTPPacket.Create(Self.Server.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $cafebabe;
  Self.Packet.CsrcIDs[1]   := $deadbeef;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := PT;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $decafbad;

  Self.Client := TIdUDPServer.Create(nil);
  Binding := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 6543; // arbitrary value
  Self.Client.Active := true;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Client.Free;
  Self.Packet.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestTIdRTPServer Private methods *******************************************

procedure TestTIdRTPServer.CheckReceivePacket(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
var
  P: TIdRTPPacket;
begin
  try
    P := TIdRTPPacket.Create(Self.Server.Profile);
    try
      P.ReadFrom(AData);
      Self.CheckHasEqualHeaders(Self.Packet, P);
    finally
      P.Free;
    end;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPServer.CheckOnRTCPRead(Sender: TObject;
                                           APacket: TIdRTCPPacket;
                                           ABinding: TIdSocketHandle);
begin
  try
    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPServer.CheckOnRTPRead(Sender: TObject;
                                          APacket: TIdRTPPacket;
                                          ABinding: TIdSocketHandle);
begin
  try
    Self.CheckHasEqualHeaders(Self.Packet, APacket);
    CheckEquals(TIdRawPayload.ClassName,
                APacket.Payload.ClassName,
                'Unexpected payload');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdRTPServer Published methods *****************************************

procedure TestTIdRTPServer.TestOnRTCPRead;
var
  S:    TStringStream;
  RTCP: TIdRTCPApplicationDefinedPacket;
begin
  Self.Server.OnRTCPRead := Self.CheckOnRTCPRead;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      RTCP := TIdRTCPApplicationDefinedPacket.Create;
      try
        RTCP.PrintOn(S);

        Self.Client.Send(Self.Server.Bindings[0].IP,
                         Self.Server.Bindings[0].Port,
                         S.DataString);

        if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
          raise Self.ExceptionType.Create(Self.ExceptionMessage);

        Check(Self.Server.MemberTable.Contains(RTCP.SyncSrcID),
              'Member not added');
        CheckEquals(Self.Client.Bindings[0].IP,
                    Self.Server.MemberTable.Member(RTCP.SyncSrcID).ControlAddress,
                    'Control address');
        CheckEquals(Self.Client.Bindings[0].Port,
                    Self.Server.MemberTable.Member(RTCP.SyncSrcID).ControlPort,
                    'Control port');
      finally
        RTCP.Free;
      end;
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestOnRTPRead;
var
  S: TStringStream;
begin
  Self.Server.OnRTPRead := Self.CheckOnRTPRead;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);

      Check(Self.Server.MemberTable.Contains(Self.Packet.SyncSrcID),
            'Member not added');
      CheckEquals(Self.Client.Bindings[0].IP,
                  Self.Server.MemberTable.Member(Self.Packet.SyncSrcID).SourceAddress,
                  'Source address');
      CheckEquals(Self.Client.Bindings[0].Port,
                  Self.Server.MemberTable.Member(Self.Packet.SyncSrcID).SourcePort,
                  'Source port');
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestOnUDPRead;
var
  S: TStringStream;
begin
  Self.Server.OnUDPRead := Self.CheckReceivePacket;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

//******************************************************************************
//* TestTIdRTPMembers                                                          *
//******************************************************************************
//* TestTIdRTPMembers Public methods *******************************************

procedure TestTIdRTPMembers.SetUp;
begin
  inherited SetUp;

  Self.Members := TIdRTPMembers.Create;
end;

procedure TestTIdRTPMembers.TearDown;
begin
  Self.Members.Free;

  inherited TearDown;
end;

//* TestTIdRTPMembers Published methods ****************************************

procedure TestTIdRTPMembers.TestAdd;
begin
  CheckEquals(0, Self.Members.Count, 'Empty table');
  Self.Members.Add($decafbad);
  CheckEquals(1, Self.Members.Count, 'SSRC not added');
end;

procedure TestTIdRTPMembers.TestContains;
begin
  Check(not Self.Members.Contains($decafbad), 'Empty table');
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added?');
end;

procedure TestTIdRTPMembers.TestRemove;
begin
  Self.Members.Add($decafbad);
  Check(Self.Members.Contains($decafbad), 'SSRC not added');
  Self.Members.Remove($decafbad);
  Check(not Self.Members.Contains($decafbad), 'SSRC not removed');
end;

//******************************************************************************
//* TestT140                                                                   *
//******************************************************************************
//* TestT140 Public methods ****************************************************

procedure TestT140.SetUp;
var
  T140: TIdRTPEncoding;
begin
  inherited SetUp;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;

  T140 := TIdRTPEncoding.Create(T140Encoding, T140ClockRate);
  try
    Self.Server.Profile.AddEncoding(T140, Self.T140PT);
  finally
    T140.Free;
  end;

  Self.Client := TIdRTPClient.Create(nil);
  Self.Client.Host    := '127.0.0.1';
  Self.Client.Port    := Self.Server.DefaultPort;
  Self.Client.Profile := Self.Server.Profile;
end;

procedure TestT140.TearDown;
begin
  Self.Client.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.StoreT140Data(Sender: TObject;
                                 APacket: TIdRTPPacket;
                                 ABinding: TIdSocketHandle);
begin
  try
    CheckEquals(TIdT140Payload.ClassName,
                APacket.Payload.ClassName,
                'Payload type');
    CheckEquals(Self.Msg,
                TIdT140Payload(APacket.Payload).Block,
                'Payload');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestT140 Published methods *************************************************

procedure TestT140.TestTransmission;
begin
  Self.Server.OnRTPRead := Self.StoreT140Data;
  Self.Server.Active := true;
  try
    Client.Send(#$00#$00 + Self.Msg, Self.T140PT);

    if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
      raise Self.ExceptionType.Create(Self.ExceptionMessage);
  finally
    Self.Server.Active := false;
  end;
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
