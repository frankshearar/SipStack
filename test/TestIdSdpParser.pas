unit TestIdSdpParser;

interface

uses
  IdSdpParser, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestStrToAddressType;
    procedure TestAddressTypeToStr;
  end;

  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsPhone;
    procedure TestIsPhoneNumber;
    procedure TestIsText;
    procedure TestParseConnection;
    procedure TestParseEmail;
    procedure TestParseEmailBracketedName;
    procedure TestParseEmailRfc822;
    procedure TestParseEmptyStream;
    procedure TestParseInvalidHeaderOrder;
    procedure TestParseMalformedVersion;
    procedure TestParseSessionEmptyString;
    procedure TestParseSessionIllegalCharacters;
    procedure TestParseInfo;
    procedure TestParseInfoIllegalCharacters;
    procedure TestParseMinimumPayload;
    procedure TestParseMalformedOrigin;
    procedure TestParseMissingOrigin;
    procedure TestParseMissingSession;
    procedure TestParseMissingVersion;
    procedure TestParseMultiplePhoneNumbers;
    procedure TestParseMultipleVersions;
    procedure TestParsePhoneNumber;
    procedure TestParsePhoneNumberWithAngleBracketsButNoName;
    procedure TestParsePhoneNumberWithStuffOutsideComment;
    procedure TestParsePhoneNumberWithUnsafeChars;
    procedure TestParseUri;
  end;

const
  MinimumPayload = 'v=0'#13#10
                 + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                 + 's=Minimum Session Info';

implementation

uses
  Classes, IdSimpleParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdpParser unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSdpParser.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestStrToAddressType;
begin
  Check(Id_IPv4 = StrToAddressType('IP4'), 'IP4');
  Check(Id_IPv6 = StrToAddressType('IP6'), 'IP6');

  try
    StrToAddressType('');
    Fail('Failed to bail out: ''''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '''' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''''');
  end;

  try
    StrToAddressType('ip4');
    Fail('Failed to bail out: ''ip4''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''ip4'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''ip4''');
  end;

  try
    StrToAddressType('IP5');
    Fail('Failed to bail out: ''IP5''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''IP5'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''IP5''');
  end;

  try
    StrToAddressType('halloo');
    Fail('Failed to bail out: ''halloo''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert ''halloo'' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''halloo''');
  end;

  try
    StrToAddressType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestAddressTypeToStr;
begin
  CheckEquals(AddressTypeIP4, AddressTypeToStr(Id_IPv4), 'Id_IPv4');
  CheckEquals(AddressTypeIP6, AddressTypeToStr(Id_IPv6), 'Id_IPv6');
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

procedure TestTIdSdpParser.TestIsPhone;
begin
  Check(not TIdSdpParser.IsPhone(''),                   '''''');
  Check(not TIdSdpParser.IsPhone('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhone('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhone('+1 '),                '+1 ');
  Check(    TIdSdpParser.IsPhone('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhone('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhone('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhone('+44-870  154 0 154'), '+44-870  154 0 154');
end;

procedure TestTIdSdpParser.TestIsPhoneNumber;
begin
  Check(not TIdSdpParser.IsPhoneNumber(''),                   '''''');
  Check(not TIdSdpParser.IsPhoneNumber('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhoneNumber('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhoneNumber('+1 '),                '+1 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+44-870  154 0 154'), '+44-870  154 0 154');

  Check(TIdSdpParser.IsPhoneNumber('"Quoted name" <+44 870 154 0 154>'),
        '"Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('aheh "Quoted name" <+44 870 154 0 154>'),
        '"aheh Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154'),
        '+44 870 154 0 154');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name)'),
        '+44 870 154 0 154 (Quoted name)');
  Check(not TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name) fink'),
        '+44 870 154 0 154 (Quoted name) fink');
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

procedure TestTIdSdpParser.TestParseConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'c=IN IP4 224.2.17.12/127');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',             Self.Payload.Connection.NetType,     'NetType');
    Check      (Id_IPv4 =         Self.Payload.Connection.AddressType, 'AddressType');
    CheckEquals('224.2.17.12/127', Self.Payload.Connection.Address,    'Address');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e=nihilistic@mystics.net');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailBracketedName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e=nihilistic@mystics.net (Apostolic alcoholics)');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    // note that "Apostolic alcoholics" is a COMMENT, not a display name.
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress.Text');
    CheckEquals('',                       Self.Payload.EmailAddress.Name, 'EmailAddress.Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailRfc822;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'e="Apostolic alcoholics" <nihilistic@mystics.net>');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals('Apostolic alcoholics <nihilistic@mystics.net>',
                Self.Payload.EmailAddress.Text,
                'EmailAddress.Text');

    CheckEquals('Apostolic alcoholics',
                Self.Payload.EmailAddress.Name,
                'EmailAddress.Name');
  finally
    S.Free;
  end;
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

procedure TestTIdSdpParser.TestParseInvalidHeaderOrder;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'u=http://127.0.0.1/'#13#10
                          + 'i=My u & i headers are swopped round');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out when headers were in the wrong order');
    except
      on E: EParser do
        CheckEquals(BadHeaderOrder,
                    E.Message,
                    'Unexpected exception');
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
        CheckEquals(Format(MalformedToken, [RSSDPSessionName, 's='#0]),
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
  S := TStringStream.Create(MinimumPayload + #13#10
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
  S := TStringStream.Create(MinimumPayload);
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

procedure TestTIdSdpParser.TestParseMalformedOrigin;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v= 0'#13#10 // note the SP after the "="
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info');
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on malformed header');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken, ['v', 'v= 0']),
                    E.Message,
                    'Unexpected exception');
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

procedure TestTIdSdpParser.TestParseMultiplePhoneNumbers;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with multiple Phone headers');
    except
      on E: EParser do
        CheckEquals(Format(TooManyHeaders, [RSSDPPhoneName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMultipleVersions;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'v=1');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with a duplicate version header');
    except
      on E: EParser do
        CheckEquals(Format(UnknownOptionalHeader, ['v=1']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumber;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('Ziggy Stardust <+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithAngleBracketsButNoName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=<+99 666 0942-3>');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('<+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithStuffOutsideComment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=+99 666 0942-3 (Ziggy Stardust) oh no! it''s garbage!');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with garbage outside the comment');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken,
                           [RSSDPPhoneName,
                            '+99 666 0942-3 (Ziggy Stardust) oh no! it''s garbage!']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithUnsafeChars;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'p=+99 666 0942-3'#0);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with unsafe characters');
    except
      on E: EParser do
        CheckEquals(Format(MalformedToken,
                           [RSSDPPhoneName,
                            'p=+99 666 0942-3'#0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload + #13#10
                          + 'u=http://127.0.0.1/');
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('http://127.0.0.1/', Self.Payload.URI.GetFullURI, 'URI');
  finally
    S.Free;
  end;
end;

initialization
  RegisterTest('IdSdpParser', Suite);
end.