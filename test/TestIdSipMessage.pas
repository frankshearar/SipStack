unit TestIdSipMessage;

interface

uses
  IdSipHeaders, IdSipMessage, TestFramework, TestFrameworkEx;

type
  TestTIdSipRequest = class(TExtendedTestCase)
  private
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure HasSipsUri;
    procedure TestIsAck;
    procedure TestIsInvite;
    procedure TestReadBody;
    procedure TestSetPath;
  end;

  TestTIdSipResponse = class(TExtendedTestCase)
  private
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignBad;
    procedure TestAsString;
    procedure TestIsFinal;
    procedure TestIsProvisional;
    procedure TestReadBody;
  end;

implementation

uses
  Classes, IdSipConsts, SysUtils, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Messages)');

  Result.AddTest(TestTIdSipRequest.Suite);
  Result.AddTest(TestTIdSipResponse.Suite);
end;

//******************************************************************************
//* TestTIdSipRequest                                                          *
//******************************************************************************
//* TestTIdSipRequest Public methods *******************************************

procedure TestTIdSipRequest.SetUp;
begin
  inherited SetUp;

  Request := TIdSipRequest.Create;  
end;

procedure TestTIdSipRequest.TearDown;
begin
  Request.Free;

  inherited TearDown;
end;

//* TestTIdSipRequest Published methods ****************************************

procedure TestTIdSipRequest.TestAssign;
var
  R: TIdSipRequest;
  I: Integer;
begin
  R := TIdSipRequest.Create;
  try
    R.SIPVersion := 'SIP/1.5';
    R.Method := 'NewMethod';
    R.RequestUri := 'sip:wintermute@tessier-ashpool.co.lu';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.ContentLength := 5;
    R.Body := 'hello';

    Self.Request.Assign(R);
    CheckEquals(R.SIPVersion, Self.Request.SipVersion, 'SIP-Version');
    CheckEquals(R.Method, Self.Request.Method, 'Method');
    CheckEquals(R.RequestUri, Self.Request.RequestUri, 'Request-URI');
    CheckEquals(R.Headers.Count, Self.Request.Headers.Count, 'Header count');

    for I := 0 to R.Headers.Count - 1 do
      CheckEquals(R.Headers.Items[0].Value,
                  Self.Request.Headers.Items[0].Value,
                  'Header ' + IntToStr(I));
  finally
    R.Free;
  end;
end;

procedure TestTIdSipRequest.TestAssignBad;
var
  P: TPersistent;
begin
  P := TPersistent.Create;
  try
    try
      Request.Assign(P);
      Fail('Failed to bail out assigning a TPersistent to a TIdSipRequest');
    except
      on EConvertError do;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipRequest.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
  Hop:      TIdSipViaHeader;
begin
  Request.Method                       := 'INVITE';
  Request.RequestUri                   := 'sip:wintermute@tessier-ashpool.co.lu';
  Request.SIPVersion                   := SIPVersion;
  Hop := Request.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
  Hop.Value                            := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Request.MaxForwards                  := 70;

  Request.Headers.Add('To').Value           := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>;tag=1928301775';
  Request.Headers.Add('From').Value         := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Request.CallID                            := 'a84b4c76e66710@gw1.leo-ix.org';
  Request.Headers.Add('CSeq').Value         := '314159 INVITE';
  Request.Headers.Add('Contact').Value      := '<sip:wintermute@tessier-ashpool.co.lu>';
  Request.Headers.Add('Content-Type').Value := 'text/plain';

  Request.ContentLength                := 29;
  Request.Body                         := 'I am a message. Hear me roar!';

  Expected := TStringList.Create;
  try
    Expected.Text := BasicRequest;

    Received := TStringList.Create;
    try
      Received.Text := Request.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseRequest(Request);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipRequest.HasSipsUri;
begin
  Self.Request.RequestUri := 'tel://999';
  Check(not Self.Request.HasSipsUri, 'tel URI');

  Self.Request.RequestUri := 'sip:wintermute@tessier-ashpool.co.lu';
  Check(not Self.Request.HasSipsUri, 'sip URI');

  Self.Request.RequestUri := 'sips:wintermute@tessier-ashpool.co.lu';
  Check(Self.Request.HasSipsUri, 'sips URI');
end;

procedure TestTIdSipRequest.TestIsAck;
begin
  Self.Request.Method := MethodAck;
  Check(Self.Request.IsAck, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsAck, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsAck, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(not Self.Request.IsAck, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsAck, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsAck, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsAck, 'XXX');
end;

procedure TestTIdSipRequest.TestIsInvite;
begin
  Self.Request.Method := MethodAck;
  Check(not Self.Request.IsInvite, MethodAck);

  Self.Request.Method := MethodBye;
  Check(not Self.Request.IsInvite, MethodBye);

  Self.Request.Method := MethodCancel;
  Check(not Self.Request.IsInvite, MethodCancel);

  Self.Request.Method := MethodInvite;
  Check(Self.Request.IsInvite, MethodInvite);

  Self.Request.Method := MethodOptions;
  Check(not Self.Request.IsInvite, MethodOptions);

  Self.Request.Method := MethodRegister;
  Check(not Self.Request.IsInvite, MethodRegister);

  Self.Request.Method := 'XXX';
  Check(not Self.Request.IsInvite, 'XXX');
end;

procedure TestTIdSipRequest.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Request.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Request.ReadBody(Str);
    CheckEquals('Negotium', Request.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

procedure TestTIdSipRequest.TestSetPath;
var
  H: TIdSipHeaders;
  P: TIdSipViaPath;
begin
  Self.Request.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';

  H := TIdSipHeaders.Create;
  try
    H.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw2.leo-ix.org;branch=z9hG4bK776asdhds';
    H.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw3.leo-ix.org;branch=z9hG4bK776asdhds';
    P := TIdSipViaPath.Create(H);
    try
      CheckEquals('SIP/2.0/TCP gw1.leo-ix.org',
                  Self.Request.Path.FirstHop.Value,
                  'Before, FirstHop');
      CheckEquals(';branch=z9hG4bK776asdhds',
                  Self.Request.Path.FirstHop.ParamsAsString,
                  'Before, FirstHop.ParamsAsString');
      CheckEquals('SIP/2.0/TCP gw1.leo-ix.org',
                  Self.Request.Path.LastHop.Value,
                  'Before, LastHop');
      CheckEquals(';branch=z9hG4bK776asdhds',
                  Self.Request.Path.LastHop.ParamsAsString,
                  'Before, LastHop.ParamsAsString');

      Self.Request.Path := P;

      CheckEquals(2, Self.Request.Path.Length, 'Path length');
      CheckEquals('SIP/2.0/TCP gw2.leo-ix.org',
                  Self.Request.Path.LastHop.Value,
                  'After, LastHop');
      CheckEquals(';branch=z9hG4bK776asdhds',
                  Self.Request.Path.LastHop.ParamsAsString,
                  'After, LastHop.ParamsAsString');
      CheckEquals('SIP/2.0/TCP gw3.leo-ix.org',
                  Self.Request.Path.FirstHop.Value,
                  'After, FirstHop');
      CheckEquals(';branch=z9hG4bK776asdhds',
                  Self.Request.Path.FirstHop.ParamsAsString,
                  'After, FirstHop.ParamsAsString');
    finally
      P.Free;
    end;
  finally
    H.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipResponse                                                         *
//******************************************************************************
//* TestTIdSipResponse Public methods ******************************************

procedure TestTIdSipResponse.SetUp;
begin
  inherited SetUp;

  Response := TIdSipResponse.Create;
end;

procedure TestTIdSipResponse.TearDown;
begin
  Response.Free;

  inherited TearDown;
end;

//* TestTIdSipResponse Published methods ***************************************

procedure TestTIdSipResponse.TestAssign;
var
  R: TIdSipResponse;
  I: Integer;
begin
  R := TIdSipResponse.Create;
  try
    R.SIPVersion := 'SIP/1.5';
    R.StatusCode := 101;
    R.StatusText := 'Hehaeha I''ll get back to you';
    R.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
    R.ContentLength := 5;
    R.Body := 'hello';

    Self.Response.Assign(R);
    CheckEquals(R.SIPVersion, Self.Response.SipVersion, 'SIP-Version');
    CheckEquals(R.StatusCode, Self.Response.StatusCode, 'Status-Code');
    CheckEquals(R.StatusText, Self.Response.StatusText, 'Status-Text');
    CheckEquals(R.Headers.Count, Self.Response.Headers.Count, 'Header count');

    for I := 0 to R.Headers.Count - 1 do
      CheckEquals(R.Headers.Items[0].Value,
                  Self.Response.Headers.Items[0].Value,
                  'Header ' + IntToStr(I));
  finally
    R.Free;
  end;
end;

procedure TestTIdSipResponse.TestAssignBad;
var
  P: TPersistent;
begin
  P := TPersistent.Create;
  try
    try
      Response.Assign(P);
      Fail('Failed to bail out assigning a TObject to a TIdSipResponse');
    except
      on EConvertError do;
    end;
  finally
    P.Free;
  end;
end;

procedure TestTIdSipResponse.TestAsString;
var
  Expected: TStrings;
  Received: TStrings;
  Parser:   TIdSipParser;
  Str:      TStringStream;
begin
  Response.StatusCode                        := 486;
  Response.StatusText                        := 'Busy Here';
  Response.SIPVersion                        := SIPVersion;
  Response.Headers.Add('Via').Value          := 'SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds';
  Response.MaxForwards                       := 70;
  Response.Headers.Add('To').Value           := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>;tag=1928301775';
  Response.Headers.Add('From').Value         := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  Response.CallID                            := 'a84b4c76e66710@gw1.leo-ix.org';
  Response.Headers.Add('CSeq').Value         := '314159 INVITE';
  Response.Headers.Add('Contact').Value      := '<sip:wintermute@tessier-ashpool.co.lu>';
  Response.Headers.Add('Content-Type').Value := 'text/plain';
  Response.ContentLength                     := 29;
  Response.Body                              := 'I am a message. Hear me roar!';

  Expected := TStringList.Create;
  try
    Expected.Text := BasicResponse;

    Received := TStringList.Create;
    try
      Received.Text := Response.AsString;

      CheckEquals(Expected, Received, '');

      Parser := TIdSipParser.Create;
      try
        Str := TStringStream.Create(Received.Text);
        try
          Parser.Source := Str;

          Parser.ParseResponse(Response);
        finally
          Str.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipResponse.TestIsFinal;
begin
  Self.Response.StatusCode := SIPTrying;
  Check(not Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPOK;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPMultipleChoices;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBadRequest;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPInternalServerError;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBusyEverywhere;
  Check(Self.Response.IsFinal, IntToStr(Self.Response.StatusCode));
end;

procedure TestTIdSipResponse.TestIsProvisional;
begin
  Self.Response.StatusCode := SIPTrying;
  Check(Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPOK;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPMultipleChoices;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBadRequest;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPInternalServerError;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));

  Self.Response.StatusCode := SIPBusyEverywhere;
  Check(not Self.Response.IsProvisional, IntToStr(Self.Response.StatusCode));
end;

procedure TestTIdSipResponse.TestReadBody;
var
  Len: Integer;
  S:   String;
  Str: TStringStream;
begin
  Response.ContentLength := 8;

  Str := TStringStream.Create('Negotium perambuians in tenebris');
  try
    Response.ReadBody(Str);
    CheckEquals('Negotium', Response.Body, 'Body');

    Len := Length(' perambuians in tenebris');
    SetLength(S, Len);
    Str.Read(S[1], Len);
    CheckEquals(' perambuians in tenebris', S, 'Unread bits of the stream');
  finally
    Str.Free;
  end;
end;

initialization
  RegisterTest('SIP Messages', Suite);
end.
