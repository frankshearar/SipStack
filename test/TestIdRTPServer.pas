unit TestIdRTPServer;

interface

uses
  Classes, IdRTPBase, IdRTP, IdRTPClient, IdRTPServer, IdSocketHandle,
  IdUDPClient, TestFramework, TestFrameworkEx, TestFrameworkSip;

type
  TestTIdRTPServer = class(TTestRTP)
  private
    Client: TIdUDPClient;
    Packet: TIdRTPPacket;
    Server: TIdRTPServer;
    procedure CheckReceivePacket(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
    procedure CheckOnRTPRead(Sender: TObject;
                             APacket: TIdRTPPacket;
                             ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOnRTPRead;
    procedure TestReceivePacket;
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
  Result.AddTest(TestT140.Suite);
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
var
  NoEncoding: TIdRTPEncoding;
  PT:         TIdRTPPayloadType;
begin
  inherited SetUp;

  PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;

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
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := PT;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  Self.Client := TIdUDPClient.Create(nil);
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;
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

      Self.Client.Send(S.DataString);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestReceivePacket;
var
  S: TStringStream;
begin
  Self.Server.OnUDPRead := Self.CheckReceivePacket;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(S.DataString);

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
//* TestT140                                                                   *
//******************************************************************************
//* TestT140 Public methods ****************************************************

procedure TestT140.SetUp;
var
  T140Encoding: TIdRTPEncoding;
begin
  inherited SetUp;

  Self.Msg := 'Goodbye, cruel world';
  Self.T140PT := 96;

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;

  T140Encoding := TIdRTPEncoding.Create(T140EncodingName, T140ClockRate);
  try
    Self.Server.Profile.AddEncoding(T140Encoding, Self.T140PT);
  finally
    T140Encoding.Free;
  end;

  Self.Client := TIdRTPClient.Create(nil);
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;
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
