unit TestIdRTPClient;

interface

uses
  Classes, IdRTP, IdRTPClient, IdRTPServer, IdSocketHandle, IdThread, SyncObjs,
  TestFrameworkRtp;

type
  TestTIdRTPClient = class(TTestRTP)
  private
    CallCount:     Cardinal;
    Client:        TIdRTPClient;
    ExpectedSeqNo: TIdRTPSequenceNo;
    Msg:           String;
    Packet:        TIdRTPPacket;
    ReceiveBuffer: TStringStream;
    ReceivedBytes: Integer;
    SendBuffer:    TStream;
    Server:        TIdRTPServer;

    procedure CheckEquals(Expected, Received: TStream); overload;
    procedure CheckSmallString(Sender: TObject;
                               APacket: TIdRTPPacket;
                               ABinding: TIdSocketHandle);
    procedure CheckString(Sender: TObject;
                          APacket: TIdRTPPacket;
                          ABinding: TIdSocketHandle);
    procedure CheckSendStream(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle);
    procedure CheckSequenceNoIncrements(Sender: TObject;
                                        APacket: TIdRTPPacket;
                                        ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSendSmallString;
    procedure TestSendString;
    procedure TestSendStream;
    procedure TestSequenceNoIncrements;
  end;

const
  DefaultTimeout = 5000; // ms

implementation

uses
  IdStack, SysUtils, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPClient unit tests');
  Result.AddTest(TestTIdRTPClient.Suite);
end;

//******************************************************************************
//* TIdTestClientThread                                                        *
//******************************************************************************
//* TIdTestClientThread Public methods *****************************************

procedure TestTIdRTPClient.SetUp;
var
  S: String;
begin
  inherited SetUp;

  Self.ReceivedBytes := 0;
  Self.CallCount := 0;

  Self.Server := TIdRTPServer.Create(nil);


  S := 'abcdefghijklmonpqrstuvwxyz0123456789';
  Self.SendBuffer := TMemoryStream.Create;
  while (Self.SendBuffer.Size < 5000) do
    Self.SendBuffer.Write(PChar(S)^, Length(S));

  Self.ReceiveBuffer := TStringStream.Create('');

  Self.Packet := TIdRTPPacket.Create(Self.Server.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := $0D;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  Self.Client := TIdRTPClient.Create(nil);

  Self.Server.DefaultPort := 5004;
  Self.Client.Host        := GStack.LocalAddress;
  Self.Client.Port        := Self.Server.DefaultPort;

  Self.Server.Active := true;

  Self.ReceiveBuffer.Seek(0, soFromBeginning);
  Self.SendBuffer.Seek(0, soFromBeginning);
end;

procedure TestTIdRTPClient.TearDown;
begin
  Self.Server.Active := false;

  Self.Client.Free;
  Self.Packet.Free;
  Self.ReceiveBuffer.Free;
  Self.SendBuffer.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestTIdRTPClient Private methods *******************************************

procedure TestTIdRTPClient.CheckEquals(Expected, Received: TStream);
const
  BufLen = 255;
var
  BytesRead:      Cardinal;
  ExpectedBuffer: array[1..BufLen] of Char;
  ReceivedBuffer: array[1..BufLen] of Char;
  TotalBytesRead: Cardinal;
begin
  CheckEquals(Expected.Size, Received.Size, 'Stream size mismatch');

  TotalBytesRead := 0;

  Expected.Seek(0, soFromBeginning);
  Received.Seek(0, soFromBeginning);
  repeat
    BytesRead := Expected.Read(ExpectedBuffer, BufLen);
    Inc(TotalBytesRead, BytesRead);

    CheckEquals(BytesRead,
                Received.Read(ReceivedBuffer, BufLen),
                'Stream size mismatch after ' + IntToStr(TotalBytesRead)
              + ' bytes read');

    CheckEquals(System.Copy(ExpectedBuffer, 1, BytesRead),
                System.Copy(ReceivedBuffer, 1, BytesRead),
                'Differing contents after ' + IntToStr(TotalBytesRead)
              + ' bytes read');
  until (BytesRead < BufLen);
end;

procedure TestTIdRTPClient.CheckSmallString(Sender: TObject;
                                            APacket: TIdRTPPacket;
                                            ABinding: TIdSocketHandle);
begin
  try
    CheckEquals(TIdRawPayload.ClassName,
                APacket.Payload.ClassName,
                'Payload type');

    CheckEquals(Self.Msg,
                TIdRawPayload(APacket.Payload).Data,
                'Payload');

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPClient.CheckString(Sender: TObject;
                                       APacket: TIdRTPPacket;
                                       ABinding: TIdSocketHandle);
var
  NewBytes: Cardinal;
begin
  try
    CheckEquals(TIdRawPayload.ClassName,
                APacket.Payload.ClassName,
                'Payload type');

    NewBytes := Length(TIdRawPayload(APacket.Payload).Data);
    CheckEquals(System.Copy(Self.Msg, Self.ReceivedBytes + 1, NewBytes),
                TIdRawPayload(APacket.Payload).Data,
                'Payload after ' + IntToStr(Self.ReceivedBytes) + ' bytes');

    Inc(Self.ReceivedBytes, Length(TIdRawPayload(APacket.Payload).Data));

    if (Self.ReceivedBytes >= Length(Self.Msg)) then begin
      Self.ThreadEvent.SetEvent;
    end;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPClient.CheckSendStream(Sender: TObject;
                                           APacket: TIdRTPPacket;
                                           ABinding: TIdSocketHandle);
begin
  try
    CheckEquals(TIdRawPayload.ClassName,
                APacket.Payload.ClassName,
                'Payload type');

    Inc(Self.ReceivedBytes, Length(TIdRawPayload(APacket.Payload).Data));
    APacket.Payload.PrintOn(Self.ReceiveBuffer);

    if (Self.ReceivedBytes >= Self.SendBuffer.Size) then begin
      CheckEquals(Self.SendBuffer, Self.ReceiveBuffer);
      Self.ThreadEvent.SetEvent;
    end;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPClient.CheckSequenceNoIncrements(Sender: TObject;
                                                     APacket: TIdRTPPacket;
                                                     ABinding: TIdSocketHandle);
begin
  try
    Inc(Self.CallCount);

    if (Self.CallCount = 1) then
      Self.ExpectedSeqNo := APacket.SequenceNo + 1
    else
      CheckEquals(Self.ExpectedSeqNo, APacket.SequenceNo, 'SequenceNo');

    if (Self.CallCount > 1) then
      Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdRTPClient Published methods *****************************************

procedure TestTIdRTPClient.TestSendSmallString;
begin
  Self.Server.OnRTPRead := Self.CheckSmallString;

  Self.Msg := 'Ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

  Self.Client.Send(Self.Msg, Self.Server.Profile.FirstFreePayloadType);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdRTPClient.TestSendString;
begin
  Self.Server.OnRTPRead := Self.CheckString;

  Self.Msg := '';
  while (Length(Self.Msg) < 500) do
    Self.Msg := Self.Msg + '0123456789';

  Self.Client.Send(Self.Msg, Self.Server.Profile.FirstFreePayloadType);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdRTPClient.TestSendStream;
begin
  Self.Server.OnRTPRead := Self.CheckSendStream;

  Self.Client.Send(Self.SendBuffer, Self.Server.Profile.FirstFreePayloadType);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout*10)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

procedure TestTIdRTPClient.TestSequenceNoIncrements;
begin
  Self.Server.OnRTPRead := Self.CheckSequenceNoIncrements;

  Self.Msg := 'Ph''nglui mglw''nafh Cthulhu R''lyeh wgah''nagl fhtagn';

  Self.Client.Send(Self.Msg, Self.Server.Profile.FirstFreePayloadType);
  Self.Client.Send(Self.Msg, Self.Server.Profile.FirstFreePayloadType);

  if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
    raise Self.ExceptionType.Create(Self.ExceptionMessage);
end;

initialization
  RegisterTest('IdRTPClient', Suite);
end.
