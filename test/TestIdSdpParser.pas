unit TestIdSdpParser;

interface

uses
  IdSdpParser, TestFramework;

type
  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseEmptyStream;
    procedure TestParseMissingOrigin;
    procedure TestParseMissingSession;
    procedure TestParseMissingVersion;
  end;

implementation

uses
  Classes, IdSimpleParser;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdpParser unit tests');
  Result.AddTest(TestTIdSdpParser.Suite);
end;

//******************************************************************************
//* TestTIdSdpParser                                                           *
//******************************************************************************
//* TestTIdSdpParser Public methods ********************************************

procedure TestTIdSdpParser.SetUp;
begin
  inherited SetUp;

  Self.P       := TIdSdpParser.Create;
  Self.Payload := TIdSdpPayload.Create;
end;

procedure TestTIdSdpParser.TearDown;
begin
  Self.Payload.Free;
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSdpParser Published methods *****************************************

procedure TestTIdSdpParser.TestParseEmptyStream;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with empty input stream');
    except
      on E: EParser do
        CheckEquals(EmptyInputStream, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingOrigin;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 's=Missing Origin. Like We Bad Syntax');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing origin-field');
    except
      on E: EParser do
        CheckEquals(MissingOrigin, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingSession;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 'i=Session Information');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing session-name-field');
    except
      on E: EParser do
        CheckEquals(MissingSessionName, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create('o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing proto-version');
    except
      on E: EParser do
        CheckEquals(MissingVersion, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

initialization
  RegisterTest('IdSdpParser', Suite);
end.