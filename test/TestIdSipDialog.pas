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
  IdSipDialog, IdSipDialogID, IdSipMessage, TestFramework, TestFrameworkSip;

type
  TestTIdSipDialog = class(TTestCaseSip)
  protected
    Dlg:                TIdSipDialog;
    ID:                 TIdSipDialogID;
    LocalSequenceNo:    Cardinal;
    LocalUri:           TIdSipURI;
    OnEstablishedFired: Boolean;
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
    procedure TestCreateWithStrings;
    procedure TestDialogID;
    procedure TestEarlyState;
    procedure TestEmptyRemoteTargetAfterResponse;
    procedure TestIsOutOfOrder;
    procedure TestIsSecure;
    procedure TestOnEstablishedFired;
    procedure TestRemoteTarget;
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
    RouteSet:         TIdSipHeaders;
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
  Classes, IdSipConsts, SysUtils, TestMessages, TypInfo;

function DialogStateToStr(const S: TIdSipDialogState): String;
begin
  Result := GetEnumName(TypeInfo(TIdSipDialogState), Integer(S));
end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipDialog unit tests');
  Result.AddTest(TestTIdSipDialog.Suite);
  Result.AddTest(TestTIdSipDialogs.Suite);
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
  Self.Res.StatusCode := SIPTrying;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo  := 13;
  Self.LocalUri         := TIdSipURI.Create('sip:case@fried.neurons.org');
  Self.RemoteSequenceNo := 42;
  Self.RemoteTarget     := TIdSipURI.Create('sip:sip-proxy1.tessier-ashpool.co.lu');
  Self.RemoteUri        := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.luna');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Dlg := TIdSipDialog.Create(Self.Req,
                                  Self.Res,
                                  Self.ID,
                                  Self.LocalSequenceNo,
                                  Self.RemoteSequenceNo,
                                  Self.LocalUri,
                                  Self.RemoteUri,
                                  Self.RemoteTarget,
                                  false,
                                  Self.RouteSet);

  Self.OnEstablishedFired := false;
end;

procedure TestTIdSipDialog.TearDown;
begin
  Self.Dlg.Free;
  Self.RouteSet.Free;
  Self.RemoteTarget.Free;
  Self.RemoteUri.Free;
  Self.LocalUri.Free;
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

procedure TestTIdSipDialog.TestCreateWithStrings;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.Req,
                           Self.Res,
                           Self.ID,
                           Self.LocalSequenceNo,
                           Self.RemoteSequenceNo,
                           Self.LocalURI,
                           Self.RemoteURI,
                           Self.RemoteTarget,
                           false,
                           Self.RouteSet);
  try
    CheckEquals(Self.LocalUri,
                D.LocalURI,
                'LocalURI');
    CheckEquals(Self.RemoteUri,
                D.RemoteURI,
                'RemoteURI');
    CheckEquals(Self.RemoteTarget,
                D.RemoteTarget,
                'RemoteTarget');
  finally
    D.Free;
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
  D:        TIdSipDialog;
  EmptyUri: TIdSipURI;
begin
  EmptyUri := TIdSipURI.Create('');
  try
    D := TIdSipDialog.Create(Self.Req,
                             Self.Res,
                             Self.ID,
                             Self.LocalSequenceNo,
                             Self.RemoteSequenceNo,
                             Self.LocalUri,
                             Self.RemoteUri,
                             EmptyUri,
                             false,
                             Self.RouteSet);
    try
      D.ReceiveResponse(Self.Res);
      CheckEquals((Self.Res.Headers[ContactHeaderFull] as TIdSipContactHeader).Address,
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
  Self.Req.CSeq.SequenceNo := Self.RemoteSequenceNo + 1;
  Check(not Self.Dlg.IsOutOfOrder(Self.Req),
        'SequenceNo > RemoteSequenceNo');

  Self.Req.CSeq.SequenceNo := Self.RemoteSequenceNo;
  Check(not Self.Dlg.IsOutOfOrder(Self.Req),
        'SequenceNo = RemoteSequenceNo');

  Self.Req.CSeq.SequenceNo := Self.RemoteSequenceNo - 1;
  Check(Self.Dlg.IsOutOfOrder(Self.Req),
        'SequenceNo < RemoteSequenceNo');
end;

procedure TestTIdSipDialog.TestIsSecure;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.Req,
                           Self.Res,
                           Self.ID,
                           Self.LocalSequenceNo,
                           Self.RemoteSequenceNo,
                           Self.LocalUri,
                           Self.RemoteUri,
                           Self.RemoteTarget,
                           true,
                           Self.RouteSet);
  try
    Check(D.IsSecure, 'Set secure');
  finally
    D.Free;
  end;

  D := TIdSipDialog.Create(Self.Req,
                           Self.Res,
                           Self.ID,
                           Self.LocalSequenceNo,
                           Self.RemoteSequenceNo,
                           Self.LocalUri,
                           Self.RemoteUri,
                           Self.RemoteTarget,
                           false,
                           Self.RouteSet);
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

procedure TestTIdSipDialog.TestRemoteTarget;
begin
  CheckEquals(Self.RemoteTarget,
              Self.Dlg.RemoteTarget,
              'RemoteTarget before response received');
end;

//******************************************************************************
//* TestTIdSipDialogs                                                          *
//******************************************************************************
//* TestTIdSipDialogs Public methods *******************************************

procedure TestTIdSipDialogs.SetUp;
begin
  inherited SetUp;

  Self.D := TIdSipDialogs.Create;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo := 13;
  Self.LocalUri        := TIdSipURI.Create('sip:case@fried.neurons.org');
  Self.LocalSequenceNo := 42;
  Self.RemoteTarget    := TIdSipURI.Create('sip:sip-proxy1.tessier-ashpool.co.lu');
  Self.RemoteUri       := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.luna');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Req := TIdSipRequest.Create;
  Self.Res := TIdSipResponse.Create;
  Self.Dlg := TIdSipDialog.Create(Self.Req,
                                  Self.Res,
                                  Self.ID,
                                  Self.LocalSequenceNo,
                                  Self.RemoteSequenceNo,
                                  Self.LocalUri,
                                  Self.RemoteUri,
                                  Self.RemoteTarget,
                                  false,
                                  Self.RouteSet);
end;

procedure TestTIdSipDialogs.TearDown;
begin
  Self.Dlg.Free;
  Req.Free;
  Res.Free;
  Self.RouteSet.Free;
  Self.RemoteTarget.Free;
  Self.RemoteUri.Free;
  Self.LocalUri.Free;
  Self.ID.Free;
  Self.D.Free;

  inherited TearDown;
end;

//* TestTIdSipDialogs Published methods ****************************************

procedure TestTIdSipDialogs.TestAddAndCount;
var
  Dlg:           TIdSipDialog;
  ID:            TIdSipDialogID;
  OriginalCount: Integer;
  RouteSet:      TIdSipHeaders;
begin
  OriginalCount := Self.D.Count;

  ID := TIdSipDialogID.Create('1', '2', '3');
  try
    RouteSet := TIdSipHeaders.Create;
    try
      Dlg := TIdSipDialog.Create(Self.Req,
                                 Self.Res,
                                 ID,
                                 1,
                                 2,
                                 'sip:localhost',
                                 'sip:remote.org',
                                 'sips:target.remote.net',
                                 false,
                                 RouteSet);
      try
        Self.D.Add(Dlg);
        CheckEquals(OriginalCount + 1, Self.D.Count, 'After one Add');
      finally
        Dlg.Free;
      end;
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
end;

procedure TestTIdSipDialogs.TestAddCopiesDialog;
var
  Dlg:      TIdSipDialog;
  ID:       TIdSipDialogID;
  RouteSet: TIdSipHeaders;
begin
  ID := TIdSipDialogID.Create('1', '2', '3');
  try
    RouteSet := TIdSipHeaders.Create;
    try
      Dlg := TIdSipDialog.Create(Self.Req,
                                 Self.Res,
                                 ID,
                                 1,
                                 2,
                                 'sip:localhost',
                                 'sip:remote.org',
                                 'sips:target.remote.net',
                                 false,
                                 RouteSet);
      try
        Self.D.Add(Dlg);
      finally
        Dlg.Free;
      end;

      // This is a sneaky test - we're implicitly testing that the list
      // COPIED Dlg. If a reference to Dlg was stored then this would
      // access violate because we'd have a dangling pointer.
      Check(ID.Equals(Self.D.Items[0].ID), 'IDs not equal');
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
end;

procedure TestTIdSipDialogs.TestDialogAt;
var
  ID2:  TIdSipDialogID;
  Dlg2: TIdSipDialog;
begin
  ID2 := TIdSipDialogID.Create('a', 'b', 'c');
  try
    Dlg2 := TIdSipDialog.Create(Self.Req,
                                Self.Res,
                                ID2,
                                Self.LocalSequenceNo,
                                Self.RemoteSequenceNo,
                                Self.LocalUri,
                                Self.RemoteUri,
                                Self.RemoteTarget,
                                false,
                                Self.RouteSet);
    try
      Self.D.Add(Dlg);
      Self.D.Add(Dlg2);

      Check(Dlg.ID.Equals(Self.D.DialogAt(Self.ID).ID),
            'Returned dialog is not Dlg');
      Check(Dlg2.ID.Equals(Self.D.DialogAt(ID2).ID),
            'Returned dialog is not Dlg2');
    finally
      Dlg2.Free;
    end;
  finally
    ID2.Free;
  end;
end;

procedure TestTIdSipDialogs.TestDialogAtString;
var
  ID2:  TIdSipDialogID;
  Dlg2: TIdSipDialog;
begin
  ID2 := TIdSipDialogID.Create('a', 'b', 'c');
  try
    Dlg2 := TIdSipDialog.Create(Self.Req,
                                Self.Res,
                                ID2,
                                Self.LocalSequenceNo,
                                Self.RemoteSequenceNo,
                                Self.LocalUri,
                                Self.RemoteUri,
                                Self.RemoteTarget,
                                false,
                                Self.RouteSet);
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
      Dlg2.Free;
    end;
  finally
    ID2.Free;
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
