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
    procedure TestIsInfo;
    procedure TestIsText;
    procedure TestIsUri;
    procedure TestParseEmptyStream;
    procedure TestParseMalformedVersion;
    procedure TestParseSessionEmptyString;
    procedure TestParseSessionIllegalCharacters;
    procedure TestParseInfo;
    procedure TestParseInfoIllegalCharacters;
    procedure TestParseMinimumPayload;
    procedure TestParseMissingOrigin;
    procedure TestParseMissingSession;
    procedure TestParseMissingVersion;
    procedure TestParseUri;
  end;

implementation

uses
  Classes, IdSimpleParser, SysUtils;

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

procedure TestTIdSdpParser.TestIsInfo;
begin
  Check(not TIdSdpParser.IsInfo(''),         '''''');
  Check(not TIdSdpParser.IsInfo('v=0'),      'v=0');
  Check(    TIdSdpParser.IsInfo('i=Heehee'), 'i=Heehee');
end;

procedure TestTIdSdpParser.TestIsText;
var
  C: Char;
begin
  Check(not TIdSdpParser.IsText(''),         '''''');
  Check(    TIdSdpParser.IsText('abc'),      'abc');
  Check(not TIdSdpParser.IsText('hello'#0),  'hello#0');
  Check(not TIdSdpParser.IsText('hello'#10), 'hello#10');
  Check(not TIdSdpParser.IsText('hello'#13), 'hello#13');

  for C := Low(Char) to High(Char) do
    if not (C in [#0, #10, #13]) then
      Check(TIdSdpParser.IsText(C), 'Checking Ord(C) = ' + IntToStr(Ord(C)));
end;

procedure TestTIdSdpParser.TestIsUri;
begin
  Check(not TIdSdpParser.IsUri(''),         '''''');
  Check(not TIdSdpParser.IsUri('v=0'),      'v=0');
  Check(    TIdSdpParser.IsUri('u=Heehee'), 'u=Heehee');
end;

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

procedure TestTIdSdpParser.TestParseMalformedVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=a'#13#10 // v must be a number
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out malformed version');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPVersionName, 'a']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSessionEmptyString;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPSessionName, '']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSessionIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's='#0);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on Session with #0');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPSessionName, #0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfo;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'i=An optional header');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('An optional header', Self.Payload.Info, 'Info');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfoIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=f00'#13#10
                          + 'i=haha'#0'haha');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on malformed information-field');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, [RSSDPInformationName, 'haha'#0'haha']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMinimumPayload;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,                                                   Self.Payload.Version,     'Version');
    CheckEquals('Minimum Session Info',                              Self.Payload.SessionName, 'SessionName');
    CheckEquals('mhandley 2890844526 2890842807 IN IP4 126.16.64.4', Self.Payload.Origin,      'Origin');
    CheckEquals('',                                                  Self.Payload.Info,        'Info');
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

procedure TestTIdSdpParser.TestParseUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'u=http://127.0.0.1/');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,                                                   Self.Payload.Version,        'Version');
    CheckEquals('Minimum Session Info',                              Self.Payload.SessionName,    'SessionName');
    CheckEquals('mhandley 2890844526 2890842807 IN IP4 126.16.64.4', Self.Payload.Origin,         'Origin');
    CheckEquals('',                                                  Self.Payload.Info,           'Info');
    CheckEquals('http://127.0.0.1/',                                 Self.Payload.URI.GetFullURI, 'URI');
  finally
    S.Free;
  end;
end;

initialization
  RegisterTest('IdSdpParser', Suite);
end.