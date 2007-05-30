{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipDialog;

interface

uses
  Classes, IdSipDialog, IdSipDialogID, IdSipMessage, TestFramework,
  TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  private
    Target: TStrings;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIntersectionOfBothSidesEmpty;
    procedure TestIntersectionOfBothSidesEmptyTargetNotEmpty;
    procedure TestIntersectionOfIgnoresWhitespace;
    procedure TestIntersectionOfLeftSideEmpty;
    procedure TestIntersectionOfNeitherSideEmpty;
    procedure TestIntersectionOfNeitherSideEmptyDisjointSets;
    procedure TestIntersectionOfRightSideEmpty;
  end;

  TestTIdSipDialog = class(TTestCaseSip)
  protected
    Dlg:                TIdSipDialog;
    ID:                 TIdSipDialogID;
    LocalSequenceNo:    Cardinal;
    LocalUri:           TIdSipURI;
    OnEstablishedFired: Boolean;
    RemoteDialog:       TIdSipDialog;
    RemoteSequenceNo:   Cardinal;
    RemoteTarget:       TIdSipURI;
    RemoteUri:          TIdSipURI;
    Req:                TIdSipRequest;
    Res:                TIdSipResponse;
    RouteSet:           TIdSipHeaders;
  private
    procedure CheckOnEstablishedFired(Sender: TIdSipDialog);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test200After180DoesntRecomputeRouteSet;
    procedure TestCopy;
    procedure TestCopyConfirmed;
    procedure TestCreateAck;
    procedure TestCreateAckUsesInviteAuthorization;
    procedure TestCreateAckUsesInviteProxyAuthorization;
    procedure TestCreateFromAnotherDialog;
    procedure TestCreateInboundDialog;
    procedure TestCreateOutboundDialog;
    procedure TestCreateRequestInDialog;
    procedure TestCreateRequestInDialogForInboundDialog;
    procedure TestCreateRequestInDialogRouteSetEmpty;
    procedure TestCreateRequestInDialogRouteSetWithLrParam;
    procedure TestCreateRequestInDialogRouteSetWithoutLrParam;
    procedure TestCreateRequestInDialogTopRouteHasForbiddenParameters;
    procedure TestDialogID;
    procedure TestEarlyState;
    procedure TestEmptyRemoteTargetAfterResponse;
    procedure TestIsOutOfOrder; virtual;
    procedure TestIsSecure;
    procedure TestOnEstablishedFired;
    procedure TestOnEstablishedFiredSubscription;
    procedure TestRemoteTarget;
    procedure TestSupportsExtension;
  end;

  TestTIdSipOutboundDialog = class(TestTIdSipDialog)
  private
    OutboundDlg: TIdSipOutboundDialog;
  public
    procedure SetUp; override;
  published
    procedure TestIsOutOfOrder; override;
    procedure TestReceiveRequestSetsRemoteSequenceNo;
  end;

  TestTIdSipDialogs = class(TTestCase)
  private
    D:                TIdSipDialogs;
    Dlg:              TIdSipDialog;
    ID:               TIdSipDialogID;
    LocalSequenceNo:  Cardinal;
    LocalUri:         TIdSipURI;
    RemoteSequenceNo: Cardinal;
    RemoteTarget:     TIdSipURI;
    RemoteUri:        TIdSipURI;
    Req:              TIdSipRequest;
    Res:              TIdSipResponse;
    RouteSet:         TIdSipHeadersFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddCopiesDialog;
    procedure TestDialogAt;
    procedure TestDialogAtString;
    procedure TestDialogAtStringUnknownID;
    procedure TestDialogAtUnknownID;
  end;

implementation

uses
  SysUtils, TestMessages, TypInfo;

function DialogStateToStr(const S: TIdSipDialogState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipDialogState), Integer(S));
end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipDialog unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipDialog.Suite);
  Result.AddTest(TestTIdSipOutboundDialog.Suite);
  Result.AddTest(TestTIdSipDialogs.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.Target := TStringList.Create;
end;

procedure TestFunctions.TearDown;
begin
  Self.Target.Free;

  inherited TearDown;
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestIntersectionOfBothSidesEmpty;
begin
  IntersectionOf(Self.Target, '', '');
  CheckEquals('',
              Self.Target.CommaText,
              'Items added to a list from empty lists');
end;

procedure TestFunctions.TestIntersectionOfBothSidesEmptyTargetNotEmpty;
begin
  Self.Target.Add('foo');
  Self.Target.Add('bar');
  Self.Target.Add('baz');

  IntersectionOf(Self.Target, '', '');
  CheckEquals('',
              Self.Target.CommaText,
              'Target list not first cleared');
end;

procedure TestFunctions.TestIntersectionOfIgnoresWhitespace;
const
  Intersection = 'gruu,tdialog';
  LeftSide     = '  gruu  , tdialog';
  RightSide    = '100rel, gruu , tdialog ';
begin
  IntersectionOf(Self.Target, LeftSide, RightSide);

  CheckEquals(Intersection,
              Self.Target.CommaText,
              'Whitespace not ignored?');
end;

procedure TestFunctions.TestIntersectionOfLeftSideEmpty;
begin
  IntersectionOf(Self.Target, '', 'gruu,tdialog');

  CheckEquals('',
              Self.Target.CommaText,
              'Right side items added');
end;

procedure TestFunctions.TestIntersectionOfNeitherSideEmpty;
const
  Intersection = 'gruu,tdialog';
  LeftSide     = Intersection + ',100rel';
  RightSide    = 'foo,bar,' + Intersection;
begin
  IntersectionOf(Self.Target, LeftSide, RightSide);

  CheckEquals(Intersection,
              Self.Target.CommaText,
              'Items not added from intersection');
end;

procedure TestFunctions.TestIntersectionOfNeitherSideEmptyDisjointSets;
const
  LeftSide = 'gruu,tdialog';
  RightSide = 'foo,bar';
begin
  IntersectionOf(Self.Target, LeftSide, RightSide);

  CheckEquals('',
              Self.Target.CommaText,
              'Items added from disjoint sets');
end;

procedure TestFunctions.TestIntersectionOfRightSideEmpty;
begin
  IntersectionOf(Self.Target, 'gruu,tdialog', '');

  CheckEquals('',
              Self.Target.CommaText,
              'Left side items added');
end;

//******************************************************************************
//* TestTIdSipDialog                                                           *
//******************************************************************************
//* TestTIdSipDialog Public methods ********************************************

procedure TestTIdSipDialog.SetUp;
begin
  inherited SetUp;

  Self.Req := TIdSipTestResources.CreateBasicRequest;
  Self.Res := TIdSipTestResources.CreateBasicResponse;
  Self.Res.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.Res.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.Res.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.ID := TIdSipDialogID.Create(Self.Req.CallID, Self.Req.From.Tag, Self.Res.ToHeader.Tag);

  Self.LocalSequenceNo  := Self.Req.CSeq.SequenceNo;
  Self.LocalUri         := Self.Req.From.Address;
  Self.RemoteSequenceNo := 0;
  Self.RemoteTarget     := Self.Res.FirstContact.Address;
  Self.RemoteUri        := Self.Res.ToHeader.Address;

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.AddInReverseOrder(Self.Res.RecordRoute);

  Self.Dlg := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);

  Self.OnEstablishedFired := false;
end;

procedure TestTIdSipDialog.TearDown;
begin
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.ID.Free;
  Self.Res.Free;
  Self.Req.Free;

  inherited TearDown;
end;

//* TestTIdSipDialog Private methods *******************************************

procedure TestTIdSipDialog.CheckOnEstablishedFired(Sender: TIdSipDialog);
begin
  Self.OnEstablishedFired := true;
end;

//* TestTIdSipDialog Published methods *****************************************

procedure TestTIdSipDialog.Test200After180DoesntRecomputeRouteSet;
var
  RouteSet180: TIdSipRecordRoutePath;
  RouteSet200: TIdSipRecordRoutePath;
  OutDlg: TIdSipDialog;
begin
  RouteSet180 := TIdSipRecordRoutePath.Create;
  try
    RouteSet200 := TIdSipRecordRoutePath.Create;
    try
      RouteSet180.Add(RecordRouteHeader).Value := '<sip:127.0.0.180>';
      RouteSet180.Add(RecordRouteHeader).Value := '<sip:127.0.1.180>';
      RouteSet200.Add(RecordRouteHeader).Value := '<sip:127.0.0.200>';

      Self.Res.RecordRoute.Clear;
      Self.Res.RecordRoute.AddInReverseOrder(RouteSet180);

      OutDlg := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
      try
        Self.Res.RecordRoute.Clear;
        Self.Res.RecordRoute.AddInReverseOrder(RouteSet200);

        OutDlg.ReceiveResponse(Self.Res);

        OutDlg.RouteSet.First;
        RouteSet180.First;
        while OutDlg.RouteSet.HasNext do begin
          Check(RouteSet180.HasNext, 'Not enough URIs in the dialog''s route set');

          CheckEquals(RouteSet180.CurrentHeader.Value,
                      OutDlg.RouteSet.CurrentHeader.Value,
                      '200 OK''s route set overwrote the dialog routeset');

          RouteSet180.Next;
          OutDlg.RouteSet.Next;
        end;
      finally
        OutDlg.Free;
      end;
    finally
      RouteSet200.Free;
    end;
  finally
    RouteSet180.Free;
  end;
end;

procedure TestTIdSipDialog.TestCopy;
var
  D: TIdSipDialog;
begin
  D := Self.Dlg.Copy;
  try
    Check(Self.Dlg.ID.Equals(D.ID),
          'ID');
    CheckEquals(Self.Dlg.LocalSequenceNo,
                D.LocalSequenceNo,
                'LocalSequenceNo');
    CheckEquals(Self.Dlg.RemoteSequenceNo,
                D.RemoteSequenceNo,
                'RemoteSequenceNo');
    CheckEquals(Self.Dlg.LocalUri,
                D.LocalUri,
                'LocalUri');
    CheckEquals(Self.Dlg.RemoteUri,
                D.RemoteUri,
                'RemoteUri');
    CheckEquals(Self.Dlg.RemoteTarget,
                D.RemoteTarget,
                'RemoteTarget');
    Check(Self.Dlg.IsEarly = D.IsEarly,
          'State');
    Check(Self.Dlg.IsSecure = D.IsSecure,
          'IsSecure');
    Check(Self.Dlg.RouteSet.Equals(D.RouteSet),
          'RouteSet');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestCopyConfirmed;
var
  D: TIdSipDialog;
begin
  Self.Res.StatusCode := SIPOK;
  Self.Dlg.ReceiveResponse(Self.Res);

  D := Self.Dlg.Copy;
  try
    Check(Self.Dlg.ID.Equals(D.ID),
          'ID');
    CheckEquals(Self.Dlg.LocalSequenceNo,
                D.LocalSequenceNo,
                'LocalSequenceNo');
    CheckEquals(Self.Dlg.RemoteSequenceNo,
                D.RemoteSequenceNo,
                'RemoteSequenceNo');
    CheckEquals(Self.Dlg.LocalUri,
                D.LocalUri,
                'LocalUri');
    CheckEquals(Self.Dlg.RemoteUri,
                D.RemoteUri,
                'RemoteUri');
    CheckEquals(Self.Dlg.RemoteTarget,
                D.RemoteTarget,
                'RemoteTarget');
    Check(Self.Dlg.IsEarly = D.IsEarly,
          'State');
    Check(Self.Dlg.IsSecure = D.IsSecure,
          'IsSecure');
    Check(Self.Dlg.RouteSet.Equals(D.RouteSet),
          'RouteSet');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateAck;
var
  Ack:        TIdSipRequest;
  LocalSeqNo: Cardinal;
begin
  LocalSeqNo := Self.Dlg.LocalSequenceNo;
  Ack := Self.Dlg.CreateAck;
  try
    CheckEquals(LocalSeqNo,
                Self.Dlg.LocalSequenceNo,
                'Sequence number must not change when generating an ACK');

    CheckNotEquals('',
                   Ack.RequestUri.Uri,
                  'ACK Request-URI cannot be the empty string');
    CheckEquals(Self.Req.LastHop.Value,
                Ack.LastHop.Value,
                'ACK topmost Via');

  finally
    Ack.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateAckUsesInviteAuthorization;
var
  Ack:      TIdSipRequest;
  AuthDlg:  TIdSipDialog;
  Expected: TIdSipHeadersFilter;
  Received: TIdSipHeadersFilter;
begin
  Self.Req.AddHeader(AuthorizationHeader).Value := 'Digest realm="tessier-ashpool.co.luna" nonce="f00" digest="f00f00"';
  Self.Req.AddHeader(AuthorizationHeader).Value := 'Digest realm="gw1.leo-ix.net" nonce="f00" digest="f00f00"';
  AuthDlg := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);

  try
    Ack := AuthDlg.CreateAck;
    try
      Expected := TIdSipHeadersFilter.Create(Self.Req.Headers,
                                             AuthorizationHeader);
      try
        Received := TIdSipHeadersFilter.Create(Ack.Headers,
                                               AuthorizationHeader);
        try
          CheckEquals(Expected, Received, 'Authorization headers');
        finally
          Received.Free;
        end;
      finally
        Expected.Free;
      end;
    finally
      Ack.Free;
    end;
  finally
    AuthDlg.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateAckUsesInviteProxyAuthorization;
var
  Ack:      TIdSipRequest;
  AuthDlg:  TIdSipDialog;
  Expected: TIdSipHeadersFilter;
  Received: TIdSipHeadersFilter;
begin
  Self.Req.AddHeader(ProxyAuthorizationHeader).Value := 'Digest realm="tessier-ashpool.co.luna" nonce="f00" digest="f00f00"';
  Self.Req.AddHeader(ProxyAuthorizationHeader).Value := 'Digest realm="gw1.leo-ix.net" nonce="f00" digest="f00f00"';

  AuthDlg := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  try
    Ack := AuthDlg.CreateAck;
    try
      Expected := TIdSipHeadersFilter.Create(Self.Req.Headers,
                                             ProxyAuthorizationHeader);
      try
        Received := TIdSipHeadersFilter.Create(Ack.Headers,
                                               ProxyAuthorizationHeader);
        try
          CheckEquals(Expected, Received, 'Proxy-Authorization headers');
        finally
          Received.Free;
        end;
      finally
        Expected.Free;
      end;
    finally
      Ack.Free;
    end;
  finally
    AuthDlg.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateFromAnotherDialog;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.Dlg);
  try
    Check(Self.Dlg.ID.Equals(D.ID),
          'ID');
    CheckEquals(Self.Dlg.LocalSequenceNo,
                D.LocalSequenceNo,
                'LocalSequenceNo');
    CheckEquals(Self.Dlg.RemoteSequenceNo,
                D.RemoteSequenceNo,
                'RemoteSequenceNo');
    CheckEquals(Self.Dlg.LocalUri,
                D.LocalUri,
                'LocalUri');
    CheckEquals(Self.Dlg.RemoteUri,
                D.RemoteUri,
                'RemoteUri');
    CheckEquals(Self.Dlg.RemoteTarget,
                D.RemoteTarget,
                'RemoteTarget');
    Check(Self.Dlg.IsSecure = D.IsSecure,
          'IsSecure');
    Check(Self.Dlg.RouteSet.Equals(D.RouteSet),
          'RouteSet');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateInboundDialog;
var
  Expected: TIdSipHeaders;
  InDlg:    TIdSipDialog;
begin
  Self.Req.AddHeaders(Self.RouteSet);
//  Check(not Self.Req.RecordRoute.IsEmpty,
//        'Sanity check - request must have Record-Route headers');

  InDlg := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  try
    Expected := TIdSipHeaders.Create;
    try
      Expected.Add(Self.RouteSet);

      Check(not InDlg.RouteSet.IsEmpty,
            'Record-Routes not used for a route set');

      Expected.First;
      InDlg.RouteSet.First;
      repeat
        CheckEquals(Expected.CurrentHeader.Value,
                    InDlg.RouteSet.CurrentHeader.Value,
                    'Route set differs');
        Expected.Next;
        InDlg.RouteSet.Next;
      until not Expected.HasNext;
    finally
      Expected.Free;
    end;
  finally
    InDlg.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateOutboundDialog;
var
  Expected: TIdSipHeaders;
  I:        Integer;
  OutDlg:   TIdSipDialog;
begin
  for I := 1 to 10 do
    Self.Res.RecordRoute.Add(RecordRouteHeader).Value := '<sip:127.0.0.'
                                                       + IntToStr(I) + '>';

  Expected := TIdSipHeaders.Create;
  try
    Expected.AddInReverseOrder(Self.Res.RecordRoute);

    OutDlg := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
    try
      CheckNotEquals(Self.Req.CSeq.SequenceNo,
                     OutDlg.RemoteSequenceNo,
                     'The dialog CANNOT set the remote sequence number, since '
                   + 'it has yet to receive an in-dialog request. RFC 3261, '
                   + 'section 12.1.2. In particular, the remote sequence number '
                   + 'mustn''t be set to the local sequence number. ');

      Check(not OutDlg.RouteSet.IsEmpty,
            'No Record-Routes added from Response');

      Expected.First;
      OutDlg.RouteSet.First;
      repeat
        CheckEquals(Expected.CurrentHeader.Value,
                    OutDlg.RouteSet.CurrentHeader.Value,
                    'Route set differs');
        Expected.Next;
        OutDlg.RouteSet.Next;
      until not Expected.HasNext;
    finally
      OutDlg.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialog;
var
  R: TIdSipRequest;
begin
  R := Self.Dlg.CreateRequest;
  try
    CheckEquals(Self.Dlg.RemoteURI,    R.ToHeader.Address, 'To URI');
    CheckEquals(Self.Dlg.ID.RemoteTag, R.ToHeader.Tag,     'To tag');
    CheckEquals(Self.Dlg.LocalURI,     R.From.Address,     'From URI');
    CheckEquals(Self.Dlg.ID.LocalTag,  R.From.Tag,         'From tag');
    CheckEquals(Self.Dlg.ID.CallID,    R.CallID,           'Call-ID');

    Check(R.HasHeader(MaxForwardsHeader), 'Max-Forwards header missing');
  finally
    R.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialogForInboundDialog;
var
  Dialog:  TIdSipDialog;
  R:       TIdSipRequest;
  SecondR: TIdSipRequest;
begin
  Dialog := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  try
    R := Dialog.CreateRequest;
    try
      CheckNotEquals(1,
                     R.CSeq.SequenceNo,
                     'Dialog didn''t select a proper initial sequence number: it was zero');

      SecondR := Dialog.CreateRequest;
      try
        CheckEquals(R.CSeq.SequenceNo + 1,
                    SecondR.CSeq.SequenceNo,
                    'Dialog didn''t monotonically increase the local sequence number');
      finally
        SecondR.Free;
      end;
    finally
      R.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialogRouteSetEmpty;
var
  R:        TIdSipRequest;
  Response: TIdSipResponse;
  Routes:   TIdSipRoutePath;
begin
  Response := TIdSipTestResources.CreateLocalLoopResponse;
  try
    Response.StatusCode := SIPTrying;
    Self.Dlg.ReceiveResponse(Response);
  finally
    Response.Free;
  end;

  Self.Dlg.RouteSet.Clear;

  R := Self.Dlg.CreateRequest;
  try
    CheckEquals(Self.Dlg.RemoteTarget, R.RequestUri, 'Request-URI');

    Routes := TIdSipRoutePath.Create(R.Headers);
    try
      Check(Routes.IsEmpty, 'Route headers are present');
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialogRouteSetWithLrParam;
var
  R:      TIdSipRequest;
  Routes: TIdSipRoutePath;
begin
  Self.Dlg.RouteSet.Clear;
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server10.biloxi.com;lr>';
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server9.biloxi.com>';
  Self.Dlg.RouteSet.Add(RouteHeader).Value := '<sip:server8.biloxi.com;lr>';

  R := Self.Dlg.CreateRequest();
  try
    CheckEquals(Self.Dlg.RemoteTarget,
                R.RequestUri,
                'Request-URI');

    Routes := TIdSipRoutePath.Create(R.Headers);
    try
      Check(Routes.Equals(Self.Dlg.RouteSet),
            'Route headers not set to the Dialog route set');
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialogRouteSetWithoutLrParam;
var
  DlgRoutes: TIdSipRoutePath;
  R:         TIdSipRequest;
  Response:  TIdSipResponse;
  Routes:    TIdSipRoutePath;
begin
  Response := TIdSipTestResources.CreateLocalLoopResponse;
  try
    Response.StatusCode := SIPTrying;
    Self.Dlg.ReceiveResponse(Response);
  finally
    Response.Free;
  end;

  R := Self.Dlg.CreateRequest;
  try
    Self.Dlg.RouteSet.First;
    CheckEquals(Self.Dlg.RouteSet.CurrentRoute.Address,
                R.RequestUri,
                'Request-URI');

    Routes := TIdSipRoutePath.Create(R.Headers);
    try
      // These are the manipulations the dialog's meant to perform on its route
      // set. Just so you know we're not fiddling our test data.
      DlgRoutes := Self.Dlg.RouteSet.GetAllButFirst;
      try
        DlgRoutes.Add(RouteHeader).Value := '<' + Self.Dlg.RemoteURI.URI + '>';

        Check(DlgRoutes.Equals(Routes), 'Routes not added correctly');
      finally
        DlgRoutes.Free;
      end;
    finally
      Routes.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateRequestInDialogTopRouteHasForbiddenParameters;
var
  Request: TIdSipRequest;
begin
  Self.Dlg.RouteSet.Items[0].Value := '<sip:127.0.0.1;transport=tcp;method=REGISTER?Subject=foo>';
  Request := Self.Dlg.CreateRequest;
  try
    CheckEquals('sip:127.0.0.1;transport=tcp',
                Request.RequestUri.Uri, 'Request-URI; SIP section 12.2.1.1');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipDialog.TestDialogID;
begin
  Check(Self.Dlg.ID.Equals(Self.ID), 'Dialog ID not set');
  CheckEquals(Self.LocalSequenceNo,
              Self.Dlg.LocalSequenceNo,
              'Local Sequence number not set');
  CheckEquals(Self.RemoteSequenceNo,
              Self.Dlg.RemoteSequenceNo,
              'Remote Sequence number not set');
  CheckEquals(Self.LocalUri,
              Self.Dlg.LocalUri,
              'Local URI not set');
  CheckEquals(Self.RemoteUri,
              Self.Dlg.RemoteUri,
              'Remote URI not set');
  CheckEquals(Self.RemoteTarget,
              Self.Dlg.RemoteTarget,
              'Remote Target not set');
  Check(not Self.Dlg.IsSecure, 'IsSecure not set');

  Check(Self.RouteSet.HasEqualValues(Self.Dlg.RouteSet),
        'Route set not set');

  Self.Dlg.RouteSet.First;
  while Self.Dlg.RouteSet.HasNext do begin
    CheckEquals(RouteHeader,
                Self.Dlg.RouteSet.CurrentHeader.Name,
                'One of the routes isn''t a Route header');
    Self.Dlg.RouteSet.Next;
  end;
end;

procedure TestTIdSipDialog.TestEarlyState;
begin
  Check(not Self.Dlg.IsEarly,
        'Before any response is received');

  Self.Res.StatusCode := SIPTrying;
  Self.Dlg.ReceiveResponse(Self.Res);
  Check(Self.Dlg.IsEarly,
        'Received provisional Response: ' + IntToStr(Self.Res.StatusCode));

  Self.Res.StatusCode := SIPOK;
  Self.Dlg.ReceiveResponse(Self.Res);
  Check(not Self.Dlg.IsEarly,
        'Received final Response: ' + IntToStr(Self.Res.StatusCode));
end;

procedure TestTIdSipDialog.TestEmptyRemoteTargetAfterResponse;
var
  Contact:  String;
  D:        TIdSipDialog;
  EmptyUri: TIdSipURI;
begin
  // If we receive a(n early) dialog-establishing response with no Contact
  // header we have no RemoteTarget value. If then we receive a 200 OK (or other
  // confirmed-dialog-establishing response WITH a Contact header, we want to
  // store that as the RemoteTarget.

  Contact := Self.Res.FirstContact.Address.Uri;
  Self.Res.RemoveAllHeadersNamed(ContactHeaderFull);
  Self.Res.StatusCode := SIPRinging;

  EmptyUri := TIdSipURI.Create('');
  try
    D := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
    try
      Self.Res.StatusCode := SIPOK;
      Self.Res.AddHeader(ContactHeaderFull).Value := Contact;

      D.ReceiveResponse(Self.Res);
      CheckEquals(Self.Res.FirstContact.Address,
                  D.RemoteTarget,
                  'RemoteTarget after response received');
    finally
      D.Free;
    end;
  finally
    EmptyUri.Free;
  end;
end;

procedure TestTIdSipDialog.TestIsOutOfOrder;
begin
  Self.Dlg.ReceiveRequest(Self.Req);

  Self.Req.CSeq.SequenceNo := Self.Dlg.RemoteSequenceNo + 1;
  Check(not Self.Dlg.IsOutOfOrder(Self.Req),
        'Request''s SequenceNo > RemoteSequenceNo but dialog think it''s out of order');

  Self.Req.CSeq.SequenceNo := Self.Dlg.RemoteSequenceNo;
  Check(Self.Dlg.IsOutOfOrder(Self.Req),
        'Request''s SequenceNo = RemoteSequenceNo but dialog think it''s out of order');

  Self.Req.CSeq.SequenceNo := Self.Dlg.RemoteSequenceNo - 1;
  Check(Self.Dlg.IsOutOfOrder(Self.Req),
        'Request''s SequenceNo < RemoteSequenceNo but dialog think it''s not out of order');
end;

procedure TestTIdSipDialog.TestIsSecure;
var
  D: TIdSipDialog;
begin
  Self.Req.FirstContact.Address.Scheme := SipsScheme;
  D := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, true);
  try
    Check(D.IsSecure, 'Set secure');
  finally
    D.Free;
  end;

  D := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  try
    Check(not D.IsSecure, 'Not set secure');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestOnEstablishedFired;
var
  OK:     TIdSipResponse;
  Invite: TIdSipRequest;
begin
  Self.Dlg.OnEstablished := Self.CheckOnEstablishedFired;

  Invite := TIdSipRequest.Create;
  try
    Invite.Method := MethodInvite;
    Self.Dlg.ReceiveRequest(Invite);
  finally
    Invite.Free;
  end;

  Check(not Self.OnEstablishedFired,
        'OnEstablished event fired prematurely');

  OK := TIdSipResponse.Create;
  try
    OK.StatusCode := SIPOK;

    Self.Dlg.ReceiveResponse(OK);
  finally
    OK.Free;
  end;

  Check(Self.OnEstablishedFired,
        'OnEstablished event didn''t fire');
end;

procedure TestTIdSipDialog.TestOnEstablishedFiredSubscription;
var
  Subscribe: TIdSipRequest;
  OK:        TIdSipResponse;
begin
  Self.Dlg.OnEstablished := Self.CheckOnEstablishedFired;

  Subscribe := TIdSipRequest.Create;
  try
    Subscribe.Method := MethodSubscribe;
    Self.Dlg.ReceiveRequest(Subscribe);
  finally
    Subscribe.Free;
  end;

  Check(not Self.OnEstablishedFired,
        'OnEstablished event fired prematurely');

  OK := TIdSipResponse.Create;
  try
    OK.StatusCode := SIPOK;

    Self.Dlg.ReceiveResponse(OK);
  finally
    OK.Free;
  end;

  Check(Self.OnEstablishedFired,
        'OnEstablished event didn''t fire');
end;

procedure TestTIdSipDialog.TestRemoteTarget;
begin
  CheckEquals(Self.RemoteTarget,
              Self.Dlg.RemoteTarget,
              'RemoteTarget before response received');
end;

procedure TestTIdSipDialog.TestSupportsExtension;
const
  ExtensionIntersection = ExtensionGruu + ',' + ExtensionTargetDialog;
  ExtensionUnknown      = 'x-unknown';
var
  D: TIdSipDialog;
begin
  Self.Req.Supported.Value := ExtensionIntersection;
  Self.Req.Supported.Values.Add(ExtensionReliableProvisional);

  Self.Res.Supported.Value := ExtensionIntersection;
  Self.Res.Supported.Values.Add(ExtensionUnknown);

  D := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  try
    Check(D.SupportsExtension(ExtensionGruu),
          '(Inbound) Dialog doesn''t support ' + ExtensionGruu);
    Check(D.SupportsExtension(ExtensionTargetDialog),
          '(Inbound) Dialog doesn''t support ' + ExtensionTargetDialog);

    Check(not D.SupportsExtension(ExtensionReliableProvisional),
          '(Inbound) Dialog supports ' + ExtensionReliableProvisional);
    Check(not D.SupportsExtension(ExtensionUnknown),
          '(Inbound) Dialog supports ' + ExtensionUnknown);
  finally
    D.Free;
  end;

  D := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
  try
    Check(D.SupportsExtension(ExtensionGruu),
          '(Outbound) Dialog doesn''t support ' + ExtensionGruu);
    Check(D.SupportsExtension(ExtensionTargetDialog),
          '(Outbound) Dialog doesn''t support ' + ExtensionTargetDialog);

    Check(not D.SupportsExtension(ExtensionReliableProvisional),
          '(Outbound) Dialog supports ' + ExtensionReliableProvisional);
    Check(not D.SupportsExtension(ExtensionUnknown),
          '(Outbound) Dialog supports ' + ExtensionUnknown);
  finally
    D.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundDialog                                                   *
//******************************************************************************
//* TestTIdSipOutboundDialog Public methods ************************************

procedure TestTIdSipOutboundDialog.SetUp;
begin
  inherited SetUp;

  Self.OutboundDlg := Self.Dlg as TIdSipOutboundDialog;
end;

procedure TestTIdSipOutboundDialog.TestIsOutOfOrder;
begin
  Self.Req.CSeq.SequenceNo := Self.OutboundDlg.LocalSequenceNo - 1;
  Check(not Self.OutboundDlg.IsOutOfOrder(Self.Req),
        'The first-received request cannot be out of order');

  inherited TestIsOutOfOrder;
end;

procedure TestTIdSipOutboundDialog.TestReceiveRequestSetsRemoteSequenceNo;
begin
  Self.Req.CSeq.SequenceNo := 42;
  Self.OutboundDlg.ReceiveRequest(Self.Req);
  Check(Self.OutboundDlg.HasReceivedRemoteRequest,
        'Dialog thinks it hasn''t received a request');
  CheckEquals(Self.Req.CSeq.SequenceNo,
              Self.OutboundDlg.RemoteSequenceNo,
              'Dialog stored the incorrect value for the remote sequence number');
end;

//******************************************************************************
//* TestTIdSipDialogs                                                          *
//******************************************************************************
//* TestTIdSipDialogs Public methods *******************************************

procedure TestTIdSipDialogs.SetUp;
begin
  inherited SetUp;

  Self.D := TIdSipDialogs.Create;

  Self.Req := TIdSipTestResources.CreateBasicRequest;
  Self.Req.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.Req.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.Req.AddHeader(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Res := TIdSipTestResources.CreateBasicResponse;
  Self.Dlg := TIdSipDialog.CreateInboundDialog(Self.Req, Self.Res, false);
  Self.ID := TIdSipDialogID.Create(Self.Dlg.ID);

  Self.LocalSequenceNo  := Self.Res.CSeq.SequenceNo;
  Self.LocalUri         := Self.Req.ToHeader.Address;
  Self.RemoteSequenceNo := Self.Req.CSeq.SequenceNo;
  Self.RemoteTarget     := Self.Req.FirstContact.Address;
  Self.RemoteUri        := Self.Req.From.Address;
  Self.RouteSet         := TIdSipHeadersFilter.Create(Self.Req.Headers, RecordRouteHeader);
end;

procedure TestTIdSipDialogs.TearDown;
begin
  Self.RouteSet.Free;
  Self.ID.Free;
  Self.Dlg.Free;
  Self.Res.Free;
  Self.Req.Free;
  Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipDialogs Published methods ****************************************

procedure TestTIdSipDialogs.TestAddAndCount;
var
  Dlg:           TIdSipDialog;
  OriginalCount: Integer;
  RouteSet:      TIdSipHeaders;
begin
  OriginalCount := Self.D.Count;

  RouteSet := TIdSipHeaders.Create;
  try
    Dlg := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
    try
      Self.D.Add(Dlg);
      CheckEquals(OriginalCount + 1, Self.D.Count, 'After one Add');
    finally
      Dlg.Free;
    end;
  finally
    RouteSet.Free;
  end;
end;

procedure TestTIdSipDialogs.TestAddCopiesDialog;
var
  Dlg:      TIdSipDialog;
  RouteSet: TIdSipHeaders;
begin
  RouteSet := TIdSipHeaders.Create;
  try
    Dlg := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
    try
      Self.D.Add(Dlg);

      Check(Self.D.Items[0] <> Dlg, 'Reference to dialog stored, not a copy');
    finally
      Dlg.Free;
    end;
  finally
    RouteSet.Free;
  end;
end;

procedure TestTIdSipDialogs.TestDialogAt;
var
  Dlg2: TIdSipDialog;
  ID2:  TIdSipDialogID;
begin
  Self.Req.CallID := '1' + Self.Req.CallID;
  Self.Res.CallID := Self.Req.CallID;
  Dlg2 := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
  try
    ID2 := TIdSipDialogID.Create(Dlg2.ID);
    try
      Self.D.Add(Dlg);
      Self.D.Add(Dlg2);

        Check(Dlg.ID.Equals(Self.D.DialogAt(Self.ID).ID),
              'Returned dialog is not Dlg');
        Check(Dlg2.ID.Equals(Self.D.DialogAt(ID2).ID),
              'Returned dialog is not Dlg2');
    finally
      ID2.Free;
    end;
  finally
    Dlg2.Free;
  end;
end;

procedure TestTIdSipDialogs.TestDialogAtString;
var
  Dlg2: TIdSipDialog;
  ID2:  TIdSipDialogID;
begin
  Self.Req.CallID := '1' + Self.Req.CallID;
  Self.Res.CallID := Self.Req.CallID;
  Dlg2 := TIdSipDialog.CreateOutboundDialog(Self.Req, Self.Res, false);
  try
    ID2 := TIdSipDialogID.Create(Dlg2.ID);
    try
      Self.D.Add(Dlg);
      Self.D.Add(Dlg2);

      Check(Dlg.ID.Equals(Self.D.DialogAt(Self.ID.CallID,
                                          Self.ID.LocalTag,
                                          Self.ID.RemoteTag).ID),
            'Returned dialog is not Dlg');
      Check(Dlg2.ID.Equals(Self.D.DialogAt(ID2.CallID,
                                           ID2.LocalTag,
                                           ID2.RemoteTag).ID),
            'Returned dialog is not Dlg2');
    finally
      ID2.Free;
    end;
  finally
    Dlg2.Free;
  end;
end;

procedure TestTIdSipDialogs.TestDialogAtStringUnknownID;
begin
  Check(Self.D.DialogAt(Self.ID.CallID + 'a',
                        Self.ID.CallID + 'b',
                        Self.ID.CallID + 'c').IsNull,
        'Null Dialog not returned');
end;

procedure TestTIdSipDialogs.TestDialogAtUnknownID;
var
  ID2:  TIdSipDialogID;
begin
  ID2 := TIdSipDialogID.Create(Self.ID.CallID + 'a',
                               Self.ID.CallID + 'b',
                               Self.ID.CallID + 'c');
  try
    Check(Self.D.DialogAt(ID2).IsNull, 'Null Dialog not returned');
  finally
    ID2.Free;
  end;
end;

initialization
  RegisterTest('Dialog', Suite);
end.
