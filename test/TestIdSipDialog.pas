unit TestIdSipDialog;

interface

uses
  IdSipDialog, IdSipMessage, TestFramework;

type
  TestTIdSipDialogID = class(TTestCase)
  published
    procedure TestCreation;
  end;

  TestTIdSipDialog = class(TTestCase)
  protected
    Req: TIdSipRequest;
    Res: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsSecure;
    procedure TestCreateRequest;
  end;

  TestTIdSipUACDialog = class(TestTIdSipDialog)
  private
    D: TIdSipUACDialog;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDialogID;
    procedure TestDialogIDToHasNoTag;
    procedure TestRecordRouteHeaders;
    procedure TestRemoteTarget;
    procedure TestSequenceNo;
    procedure TestUri;
  end;

  TestTIdSipUASDialog = class(TestTIdSipDialog)
  private
    D: TIdSipUASDialog;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDialogID;
    procedure TestDialogIDFromHasNoTag;
    procedure TestEarlyState;
    procedure TestRemoteTarget;
    procedure TestRecordRouteHeaders;
    procedure TestSequenceNo;
    procedure TestUri;
  end;

implementation

uses
  Classes, IdSipParser, SysUtils, TestMessages, TypInfo;

function DialogStateToStr(const S: TIdSipDialogState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipDialogState), Integer(S));
end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Foo unit tests');
  Result.AddTest(TestTIdSipDialogID.Suite);
  Result.AddTest(TestTIdSipDialog.Suite);
  Result.AddTest(TestTIdSipUACDialog.Suite);
  Result.AddTest(TestTIdSipUASDialog.Suite);
end;

//******************************************************************************
//* TestTIdSipDialogID                                                         *
//******************************************************************************
//* TestTIdSipDialogID Published methods ***************************************

procedure TestTIdSipDialogID.TestCreation;
var
  Dlg: TIdSipDialogID;
begin
  Dlg := TIdSipDialogID.Create('1', '2', '3');
  try
    CheckEquals('1', Dlg.CallID,    'CallID');
    CheckEquals('2', Dlg.LocalTag,  'LocalTag');
    CheckEquals('3', Dlg.RemoteTag, 'RemoteTag');
  finally
    Dlg.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipDialog                                                           *
//******************************************************************************
//* TestTIdSipDialog Public methods ********************************************

procedure TestTIdSipDialog.SetUp;
var
  P: TIdSipParser;
  S: TStringStream;
begin
  inherited SetUp;

  S := TStringStream.Create(BasicRequest);
  try
    P := TIdSipParser.Create;
    try
      P.Source := S;

      Self.Req := P.ParseAndMakeMessage as TIdSipRequest;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(BasicResponse);
  try
    P := TIdSipParser.Create;
    try
      P.Source := S;

      Self.Res := P.ParseAndMakeMessage as TIdSipResponse;
      Self.Res.StatusCode := SIPTrying;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSipDialog.TearDown;
begin
  Self.Res.Free;
  Self.Req.Free;

  inherited TearDown;
end;

//* TestTIdSipDialog Published methods *****************************************

procedure TestTIdSipDialog.TestIsSecure;
var
  D: TIdSipDialog;
begin
  Self.Req.RequestUri := 'sip:wintermute@tessier-ashpool.co.lu';
  D := TIdSipDialog.Create(Self.Req, false);
  try
    Check(not D.IsSecure, 'SIP Request-URI, not received over TLS');
  finally
    D.Free;
  end;

  Self.Req.RequestUri := 'sip:wintermute@tessier-ashpool.co.lu';
  D := TIdSipDialog.Create(Self.Req, true);
  try
    Check(not D.IsSecure, 'SIP Request-URI, received over TLS');
  finally
    D.Free;
  end;

  Self.Req.RequestUri := 'sips:wintermute@tessier-ashpool.co.lu';
  D := TIdSipDialog.Create(Self.Req, false);
  try
    Check(not D.IsSecure, 'SIPS Request-URI, not received over TLS');
  finally
    D.Free;
  end;

  Self.Req.RequestUri := 'sips:wintermute@tessier-ashpool.co.lu';
  D := TIdSipDialog.Create(Self.Req, true);
  try
    Check(D.IsSecure, 'SIPS Request-URI, received over TLS');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequest;
var
  D: TIdSipDialog;
begin
  Self.Req.RequestUri := 'sip:wintermute@tessier-ashpool.co.lu';
  D := TIdSipDialog.Create(Self.Req, false);
  try
  finally
    D.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUACDialog                                                        *
//******************************************************************************
//* TestTIdSipUACDialog Public methods *****************************************

procedure TestTIdSipUACDialog.SetUp;
begin
  inherited SetUp;

  Self.D := TIdSipUACDialog.Create(Self.Req, false);
end;

procedure TestTIdSipUACDialog.TearDown;
begin
  Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipUACDialog Published methods **************************************

procedure TestTIdSipUACDialog.TestDialogID;
begin
  CheckEquals(Self.Req.CallID,       Self.D.ID.CallID,    'CallID');
  CheckEquals(Self.Req.From.Tag,     Self.D.ID.LocalTag,  'LocalTag');
  CheckEquals(Self.Req.ToHeader.Tag, Self.D.ID.RemoteTag, 'RemoteTag');
end;

procedure TestTIdSipUACDialog.TestDialogIDToHasNoTag;
var
  D: TIdSipUACDialog;
begin
  Self.Req.ToHeader.Value := 'Case <sip:case@fried.neurons.org>';
  D := TIdSipUACDialog.Create(Self.Req, false);
  try
    CheckEquals('', D.ID.RemoteTag, 'LocalTag value with no To tag');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipUACDialog.TestRecordRouteHeaders;
var
  D: TIdSipUACDialog;
begin
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5000;foo>';
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5001>';
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5002>';

  D := TIdSipUACDialog.Create(Self.Req, false);
  try
    CheckEquals(3, D.RouteSet.Count, 'Incorrect number of Record-Route headers');
    CheckEquals('<sip:127.0.0.1:5002>',     D.RouteSet.Items[0].Value, '1st Record-Route header');
    CheckEquals('<sip:127.0.0.1:5001>',     D.RouteSet.Items[1].Value, '2nd Record-Route header');
    CheckEquals('<sip:127.0.0.1:5000;foo>', D.RouteSet.Items[2].Value, '3rd Record-Route header');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipUACDialog.TestRemoteTarget;
begin
  CheckEquals('',
              Self.D.RemoteTarget.GetFullURI,
              'RemoteTarget before response received');

  Self.D.HandleMessage(Self.Res);
  CheckEquals((Self.Res.Headers[ContactHeaderFull] as TIdSipContactHeader).Address.GetFullUri,
              Self.D.RemoteTarget.GetFullURI,
              'RemoteTarget after response received');
end;

procedure TestTIdSipUACDialog.TestSequenceNo;
begin
  CheckEquals(Self.Req.CSeq.SequenceNo, Self.D.LocalSequenceNo,  'LocalSequenceNo');
  CheckEquals(0,                        Self.D.RemoteSequenceNo, 'RemoteSequenceNo');
end;

procedure TestTIdSipUACDialog.TestUri;
begin
  CheckEquals(Self.Req.From.Address.GetFullUri,
              Self.D.LocalURI.GetFullUri,
              'LocalUri');

  CheckEquals(Self.Req.ToHeader.Address.GetFullUri,
              Self.D.RemoteURI.GetFullUri,
              'RemoteUri');
end;

//******************************************************************************
//* TestTIdSipUASDialog                                                        *
//******************************************************************************
//* TestTIdSipUASDialog Public methods *****************************************

procedure TestTIdSipUASDialog.SetUp;
begin
  inherited SetUp;

  Self.D := TIdSipUASDialog.Create(Self.Req, false);
end;

procedure TestTIdSipUASDialog.TearDown;
begin
  Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipUASDialog Published methods **************************************

procedure TestTIdSipUASDialog.TestDialogID;
begin
  CheckEquals(Self.Req.CallID,       Self.D.ID.CallID,    'CallID');
  CheckEquals(Self.Req.ToHeader.Tag, Self.D.ID.LocalTag,  'LocalTag');
  CheckEquals(Self.Req.From.Tag,     Self.D.ID.RemoteTag, 'RemoteTag');
end;

procedure TestTIdSipUASDialog.TestDialogIDFromHasNoTag;
var
  D: TIdSipUASDialog;
begin
  Self.Req.From.Value := 'Case <sip:case@fried.neurons.org>';
  D := TIdSipUASDialog.Create(Self.Req, false);
  try
    CheckEquals('', D.ID.RemoteTag, 'LocalTag value with no From tag');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipUASDialog.TestEarlyState;
begin
  Check(Self.D.IsEarly,
        'Response initial state: ' + IntToStr(Self.Res.StatusCode));

  Self.Res.StatusCode := SIPTrying;
  Self.D.HandleMessage(Self.Res);
  Check(Self.D.IsEarly,
        'Received provisional Response: ' + IntToStr(Self.Res.StatusCode));

  Self.Res.StatusCode := SIPOK;
  Self.D.HandleMessage(Self.Res);
  Check(not Self.D.IsEarly,
        'Received final Response: ' + IntToStr(Self.Res.StatusCode));
end;

procedure TestTIdSipUASDialog.TestRemoteTarget;
begin
  CheckEquals((Self.Req.Headers[ContactHeaderFull] as TIdSipContactHeader).Address.GetFullURI,
              Self.D.RemoteTarget.GetFullURI,
              'Remote target');
end;

procedure TestTIdSipUASDialog.TestRecordRouteHeaders;
var
  D: TIdSipUASDialog;
begin
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5000;foo>';
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5001>';
  Self.Req.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:5002>';

  D := TIdSipUASDialog.Create(Self.Req, false);
  try
    CheckEquals(3, D.RouteSet.Count, 'Incorrect number of Record-Route headers');
    CheckEquals('<sip:127.0.0.1:5000;foo>', D.RouteSet.Items[0].Value, '1st Record-Route header');
    CheckEquals('<sip:127.0.0.1:5001>',     D.RouteSet.Items[1].Value, '2nd Record-Route header');
    CheckEquals('<sip:127.0.0.1:5002>',     D.RouteSet.Items[2].Value, '3rd Record-Route header');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipUASDialog.TestSequenceNo;
begin
  CheckEquals(0,                        Self.D.LocalSequenceNo,  'LocalSequenceNo');
  CheckEquals(Self.Req.CSeq.SequenceNo, Self.D.RemoteSequenceNo, 'RemoteSequenceNo');
end;

procedure TestTIdSipUASDialog.TestUri;
begin
  CheckEquals(Self.Req.ToHeader.Address.GetFullUri,
              Self.D.LocalURI.GetFullUri,
              'LocalUri');

  CheckEquals(Self.Req.From.Address.GetFullUri,
              Self.D.RemoteURI.GetFullUri,
              'RemoteUri');
end;

initialization
  RegisterTest('Foo', Suite);
end.
