unit TestIdRTPServer;

interface

uses
  Classes, IdRTP, IdRTPServer, IdSocketHandle, IdUDPServer, SyncObjs,
  TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TestTIdRTPServer = class(TTestRTP)
  private
    Encoding: TIdRTPPayload;
    Client:   TIdRTPServer;
    Packet:   TIdRTPPacket;
    Server:   TIdRTPServer;
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

  TestT140 = class(TThreadingTestCase)
  private
    Client:    TIdRTPServer;
    Msg:       String;
    RTCPEvent: TEvent;
    Server:    TIdRTPServer;
    T140PT:    TIdRTPPayloadType;

    procedure ReceiveAnyOldJunk(Sender: TObject;
                                APacket: TIdRTCPPacket;
                                ABinding: TIdSocketHandle);
    procedure StoreT140Data(Sender: TObject;
                            APacket: TIdRTPPacket;
                            ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTransmission;
  end;

implementation

uses
  DateUtils, IdGlobal, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');
  Result.AddTest(TestTIdRTPServer.Suite);
//  Result.AddTest(TestT140.Suite);
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
var
  Binding:    TIdSocketHandle;
  NoEncoding: TIdRTPPayload;
  PT:         TIdRTPPayloadType;
begin
  inherited SetUp;

  PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Binding := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5004;

  Self.Packet := TIdRTPPacket.Create(Self.Server.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := false;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $cafebabe;
  Self.Packet.CsrcIDs[1]   := $deadbeef;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := PT;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $decafbad;

  Self.Client := TIdRTPServer.Create(nil);
  Binding := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 6543; // arbitrary value

  NoEncoding := TIdRTPPayload.CreatePayload('No encoding/0');
  try
    Self.Server.Profile.AddEncoding(NoEncoding, PT);
  finally
    NoEncoding.Free;
  end;

  Self.Encoding := TIdT140Payload.CreatePayload(T140Encoding + '/' + IntToStr(T140ClockRate));
  Self.Server.Profile.AddEncoding(Self.Encoding, PT + 1);
  Self.Client.Profile.AddEncoding(Self.Encoding, PT + 1);

  Self.Client.Active := true;
  Self.Server.Active := true;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Server.Active := false;
  Self.Client.Active := false;

  Self.Encoding.Free;
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
  RTCP: TIdRTCPApplicationDefined;
begin
  Self.Server.OnRTCPRead := Self.CheckOnRTCPRead;
  S := TStringStream.Create('');
  try
    RTCP := TIdRTCPApplicationDefined.Create;
    try
      RTCP.PrintOn(S);

      Self.Client.Send(Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port,
                       S.DataString);

      Self.WaitForSignaled;

      Check(Self.Server.Session.IsMember(RTCP.SyncSrcID),
            'Member not added');
      CheckEquals(Self.Client.Bindings[0].IP,
                  Self.Server.Session.Member(RTCP.SyncSrcID).ControlAddress,
                  'Control address');
      CheckEquals(Self.Client.Bindings[0].Port,
                  Self.Server.Session.Member(RTCP.SyncSrcID).ControlPort,
                  'Control port');
    finally
      RTCP.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPServer.TestOnRTPRead;
var
  S: TStringStream;
begin
  Self.Server.OnRTPRead := Self.CheckOnRTPRead;
  S := TStringStream.Create('');
  try
    Self.Packet.PrintOn(S);

    Self.Client.Send(Self.Server.Bindings[0].IP,
                     Self.Server.Bindings[0].Port,
                     S.DataString);

    Self.WaitForSignaled;

    Check(Self.Server.Session.IsMember(Self.Packet.SyncSrcID),
          'Member not added');
    CheckEquals(Self.Client.Bindings[0].IP,
                Self.Server.Session.Member(Self.Packet.SyncSrcID).SourceAddress,
                'Source address');
    CheckEquals(Self.Client.Bindings[0].Port,
                Self.Server.Session.Member(Self.Packet.SyncSrcID).SourcePort,
                'Source port');
  finally
    S.Free;
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

      Self.WaitForSignaled;
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

//******************************************************************************
//* TestT140                                                                   *
//******************************************************************************
//* TestT140 Public methods ****************************************************

procedure TestT140.SetUp;
var
  Binding: TIdSocketHandle;
  T140:    TIdRTPPayload;
begin
  inherited SetUp;

  Self.DefaultTimeout := 5000;
  Self.RTCPEvent := TSimpleEvent.Create;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Binding      := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 5004;

  Self.Client  := TIdRTPServer.Create(nil);
  Binding      := Self.Client.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := Self.Server.DefaultPort + 2;

  T140 := TIdT140Payload.CreatePayload(T140Encoding + '/' + IntToStr(T140ClockRate));
  try
    Self.Server.Profile.AddEncoding(T140, Self.T140PT);
    Self.Client.Profile.AddEncoding(T140, Self.T140PT);
  finally
    T140.Free;
  end;

  Self.Server.Active := true;
  Self.Client.Active := true;
end;

procedure TestT140.TearDown;
begin
  Self.Client.Active := false;
  Self.Server.Active := false;

  Self.Client.Free;
  Self.Server.Free;
  Self.RTCPEvent.Free;

  inherited TearDown;
end;

//* TestT140 Private methods ***************************************************

procedure TestT140.ReceiveAnyOldJunk(Sender: TObject;
                                     APacket: TIdRTCPPacket;
                                     ABinding: TIdSocketHandle);
begin
  Self.RTCPEvent.SetEvent;
end;

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
var
  Payload: TIdT140Payload;
  Session: TIdRTPSession;
begin
  Self.ExceptionMessage := 'Waiting for RTCP to Server';
  Self.Server.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Client.OnRTCPRead := Self.ReceiveAnyOldJunk;
  Self.Server.OnRTPRead := Self.StoreT140Data;

  Self.Server.JoinSession(Self.Client.Bindings[0].IP,
                          Self.Client.Bindings[0].Port);
  Self.WaitForSignaled(Self.RTCPEvent);

  Self.ExceptionMessage := 'Waiting for RTCP to Client';
  Session := Self.Client.JoinSession(Self.Server.Bindings[0].IP,
                                     Self.Server.Bindings[0].Port);
  Self.WaitForSignaled(Self.RTCPEvent);

  Self.ExceptionMessage := 'Waiting for RFC 2793 data';
  Payload := Self.Client.Profile.EncodingFor(Self.T140PT).Clone as TIdT140Payload;
  try
    Payload.Block := Self.Msg;
    Payload.StartTime := Now;
    Session.SendDataTo(Payload,
                       Self.Server.Bindings[0].IP,
                       Self.Server.Bindings[0].Port);
  finally
    Payload.Free;
  end;

  Self.WaitForSignaled;
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
