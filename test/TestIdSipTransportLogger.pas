{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipTransportLogger;

interface

uses
  Classes, IdRoutingTable, IdSipLocation, IdSipMessage, IdSipMockTransport,
  IdSipTransportLogger, TestFramework;

type
  TestTIdSipTransportLogger = class(TTestCase)
  private
    ArbDest:      TIdSipLocation;
    Logger:       TIdSipTransportLogger;
    Request:      TIdSipRequest;
    Response:     TIdSipResponse;
    RoutingTable: TIdRoutingTable;
    Transport:    TIdSipMockTransport;
    LogStream:    TStringStream;

    function FirstLineOfLog: String; overload;
    function FirstLineOfLog(S: TStringStream): String; overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckContains(const Substring: String;
                            const S: String);
    procedure CheckContainsDate(const Data: String;
                                Direction: TIdMessageDirection);
    procedure CheckDirection(const LogLine: String;
                             Direction: TIdMessageDirection);
    procedure CheckMessageLogged(Msg: TIdSipMessage;
                             Direction: TIdMessageDirection);
  published
    procedure TestLogException;
    procedure TestLogReceiveRequest;
    procedure TestLogReceiveResponse;
    procedure TestLogRejectedMessage;
    procedure TestLogSendRequest;
    procedure TestLogSendRequestTwice;
    procedure TestLogSendResponse;
  end;

implementation

uses
  IdSipTransport, SysUtils, TestFrameworkSip;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipTransportLogger unit tests');
  Result.AddTest(TestTIdSipTransportLogger.Suite);
end;

//******************************************************************************
//* TestTIdSipTransportLogger                                                  *
//******************************************************************************
//* TestTIdSipTransportLogger Public methods ***********************************

procedure TestTIdSipTransportLogger.SetUp;
var
  Bindings: TIdSipLocations;
begin
  inherited SetUp;

  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUdpTransport);

  Self.ArbDest := TIdSipLocation.Create;
  Self.ArbDest.IPAddress := '127.0.0.1';
  Self.ArbDest.Port      := 5060;
  Self.ArbDest.Transport := 'UDP';
  Self.LogStream    := TStringStream.Create('');
  Self.Request      := TIdSipTestResources.CreateBasicRequest;
  Self.Response     := TIdSipTestResources.CreateBasicResponse;
  Self.RoutingTable := TIdMockRoutingTable.Create;
  Self.Transport    := TIdSipMockUdpTransport.Create;
  Self.Transport.RoutingTable := Self.RoutingTable;

  Bindings := TIdSipLocations.Create;
  try
    Self.Transport.LocalBindings(Bindings);
    Self.Response.LastHop.SentBy := Bindings[0].IPAddress;
    Self.Response.LastHop.Port   := Bindings[0].Port;
  finally
    Bindings.Free;
  end;

  Self.Logger := TIdSipTransportLogger.Create;
  Self.Logger.OutputStream := Self.LogStream;
  Self.Logger.LogTransport(Self.Transport);
end;

procedure TestTIdSipTransportLogger.TearDown;
begin
  Self.Transport.Free;
  Self.RoutingTable.Free;
  Self.Response.Free;
  Self.Request.Free;
  Self.LogStream.Free;
  Self.Logger.Free;
  Self.ArbDest.Free;

  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited TearDown;
end;

procedure TestTIdSipTransportLogger.CheckContains(const Substring: String;
                                                  const S: String);
begin
  Check(Pos(S, Substring) >= 0,
        'String doesn''t contain substring ''' + SubString + '''');
end;

procedure TestTIdSipTransportLogger.CheckContainsDate(const Data: String;
                                                      Direction: TIdMessageDirection);
var
  Date:          String;
  OldDateFormat: String;
begin
  Date := Trim(Copy(Data,
                    Length(DirectionToStr(Direction)) + 1,
                    Length(Data)));

  OldDateFormat := ShortDateFormat;
  try
    ShortDateFormat := 'yyyy/mm/dd hh:mm:ss.zzz';

    try
      StrToDateTime(Date)
    except
      on EConvertError do
        Fail('First line doesn''t contain a valid date & time (' + Data + ')');
    end;
  finally
    ShortDateFormat := OldDateFormat;
  end;
end;

procedure TestTIdSipTransportLogger.CheckDirection(const LogLine: String;
                                                   Direction: TIdMessageDirection);
var
  Token: String;
begin
  Token := DirectionToStr(Direction);

  CheckEquals(Token,
              Copy(LogLine, 1, Length(Token)),
              'Message direction token missing');
end;

procedure TestTIdSipTransportLogger.CheckMessageLogged(Msg: TIdSipMessage;
                                                       Direction: TIdMessageDirection);
var
  FirstLine: String;
begin
  Check(Pos(Msg.AsString, Self.LogStream.DataString) > 0,
        'No request logged');

  FirstLine := Self.FirstLineOfLog;
  CheckDirection(FirstLine, Direction);

  CheckContainsDate(FirstLine, Direction);
end;

//* TestTIdSipTransportLogger Private methods **********************************

function TestTIdSipTransportLogger.FirstLineOfLog: String;
begin
  Result := Self.FirstLineOfLog(Self.LogStream);
end;

function TestTIdSipTransportLogger.FirstLineOfLog(S: TStringStream): String;
var
  LogStrings: TStrings;
begin
  LogStrings := TStringList.Create;
  try
    LogStrings.Text := Self.LogStream.DataString;

    Result := LogStrings[0];
  finally
    LogStrings.Free;
  end;
end;

//* TestTIdSipTransportLogger Published methods ********************************

procedure TestTIdSipTransportLogger.TestLogException;
var
  ExType:    ExceptClass;
  ExMsg:     String;
  FirstLine: String;
  Msg:       TIdSipMessage;
  Reason:    String;
begin
  ExType := EConvertError;
  ExMsg  := 'Access violation at f00L reading $decafbad';
  Reason := 'Just for kicks';

  Msg := TIdSipTestResources.CreateBasicRequest;
  try
    Self.Transport.FireOnException(Msg, ExType, ExMsg, Reason);

    FirstLine := Self.FirstLineOfLog;
    CheckDirection(FirstLine, dirError);
    CheckContainsDate(FirstLine, dirError);
    CheckContains(FirstLine, ExType.ClassName);
    CheckContains(FirstLine, ExMsg);
    CheckContains(FirstLine, Reason);
  finally
    Msg.Free;
  end;
end;

procedure TestTIdSipTransportLogger.TestLogReceiveRequest;
begin
  // What the MockTransport adds.
  Self.Request.LastHop.Received := Self.Transport.PeerIP;
  Self.Transport.FireOnRequest(Self.Request);

  Self.CheckMessageLogged(Self.Request, dirIn);
end;

procedure TestTIdSipTransportLogger.TestLogReceiveResponse;
begin
  Self.Transport.FireOnResponse(Self.Response);

  Self.CheckMessageLogged(Self.Response, dirIn);
end;

procedure TestTIdSipTransportLogger.TestLogRejectedMessage;
var
  FirstLine: String;
  Reason: String;
begin
  Reason := 'Just for kicks';

  Self.Transport.FireOnRejectedMessage(Self.Request, Reason);

  FirstLine := Self.FirstLineOfLog;

  CheckDirection(FirstLine, dirError);
  CheckContainsDate(FirstLine, dirError);
  CheckContains(FirstLine, Reason);
  CheckContains(FirstLine, RejectedMsg);

  Self.CheckMessageLogged(Self.Request, dirError);
end;

procedure TestTIdSipTransportLogger.TestLogSendRequest;
begin
  Self.Transport.Send(Self.Request, Self.ArbDest);

  Self.CheckMessageLogged(Self.Request, dirOut);
end;

procedure TestTIdSipTransportLogger.TestLogSendRequestTwice;
var
  MsgLen: Integer;
begin
  Self.Transport.Send(Self.Request, Self.ArbDest);

  MsgLen := Length(Self.LogStream.DataString);

  Self.Transport.Send(Self.Request, Self.ArbDest);
  CheckEquals(2*MsgLen,
              Length(Self.LogStream.DataString),
              '2nd message not logged');
end;

procedure TestTIdSipTransportLogger.TestLogSendResponse;
begin
  Self.Transport.Send(Self.Response, Self.ArbDest);

  Self.CheckMessageLogged(Self.Response, dirOut);
end;

initialization
  RegisterTest('Message Logger', Suite);
end.
