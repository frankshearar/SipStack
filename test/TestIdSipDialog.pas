unit TestIdSipDialog;

interface

uses
  IdSipDialog, IdSipDialogID, IdSipHeaders, IdSipMessage, IdURI, TestFramework,
  TestFrameworkSip;

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
    procedure TestCreateFromAnotherDialog;
    procedure TestCreateWithStrings;
    procedure TestDialogID;
    procedure TestEarlyState;
    procedure TestEmptyRemoteTargetAfterResponse;
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
var
  P: TIdSipParser;
begin
  inherited SetUp;

  P := TIdSipParser.Create;
  try
    Self.Req := P.ParseAndMakeRequest(BasicRequest);

    Self.Res := P.ParseAndMakeResponse(BasicResponse);
    Self.Res.StatusCode := SIPTrying;
  finally
    P.Free;
  end;

  Self.ID := TIdSipDialogID.Create('1', '2', '3');

  Self.LocalSequenceNo := 13;
  Self.LocalUri        := TIdSipURI.Create('sip:case@fried.neurons.org');
  Self.LocalSequenceNo := 42;
  Self.RemoteTarget    := TIdSipURI.Create('sip:sip-proxy1.tessier-ashpool.co.lu');
  Self.RemoteUri       := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.lu');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Dlg := TIdSipDialog.Create(Self.ID,
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

procedure TestTIdSipDialog.TestCreateFromAnotherDialog;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.Dlg);
  try
    Check(Self.Dlg.ID.IsEqualTo(D.ID),
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
    Check(Self.Dlg.RouteSet.IsEqualTo(D.RouteSet),
          'RouteSet');
  finally
    D.Free;
  end;
end;

procedure TestTIdSipDialog.TestCreateWithStrings;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.ID,
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
  Check(Self.Dlg.ID.IsEqualTo(Self.ID), 'Dialog ID not set');
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
  Check(Self.Dlg.RouteSet.IsEqualTo(Self.RouteSet), 'Route set not set');
end;

procedure TestTIdSipDialog.TestEarlyState;
begin
  Check(not Self.Dlg.IsEarly,
        'Before any response is received');

  Self.Res.StatusCode := SIPTrying;
  Self.Dlg.HandleMessage(Self.Res);
  Check(Self.Dlg.IsEarly,
        'Received provisional Response: ' + IntToStr(Self.Res.StatusCode));

  Self.Res.StatusCode := SIPOK;
  Self.Dlg.HandleMessage(Self.Res);
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
    D := TIdSipDialog.Create(Self.ID,
                             Self.LocalSequenceNo,
                             Self.RemoteSequenceNo,
                             Self.LocalUri,
                             Self.RemoteUri,
                             EmptyUri,
                             false,
                             Self.RouteSet);
    try
      D.HandleMessage(Self.Res);
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

procedure TestTIdSipDialog.TestIsSecure;
var
  D: TIdSipDialog;
begin
  D := TIdSipDialog.Create(Self.ID,
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

  D := TIdSipDialog.Create(Self.ID,
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
    Self.Dlg.HandleMessage(Invite);
  finally
    Invite.Free;
  end;

  Check(not Self.OnEstablishedFired, 'OnEstablished event fired prematurely');

  OK := TIdSipResponse.Create;
  try
    OK.StatusCode := SIPOK;

    Self.Dlg.HandleMessage(OK);
  finally
    OK.Free;
  end;

  Check(Self.OnEstablishedFired, 'OnEstablished event didn''t fire');
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
  Self.RemoteUri       := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.lu');

  Self.RouteSet := TIdSipHeaders.Create;
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.RouteSet.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:8000>';

  Self.Dlg := TIdSipDialog.Create(Self.ID,
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
      Dlg := TIdSipDialog.Create(ID,
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
      Dlg := TIdSipDialog.Create(ID,
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
      Check(ID.IsEqualTo(Self.D.Items[0].ID), 'IDs not equal');
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
    Dlg2 := TIdSipDialog.Create(ID2, Self.LocalSequenceNo, Self.RemoteSequenceNo, Self.LocalUri, Self.RemoteUri, Self.RemoteTarget, false, Self.RouteSet);
    try
      Self.D.Add(Dlg);
      Self.D.Add(Dlg2);

      Check(Dlg.ID.IsEqualTo(Self.D.DialogAt(Self.ID).ID), 'Returned dialog is not Dlg');
      Check(Dlg2.ID.IsEqualTo(Self.D.DialogAt(ID2).ID),    'Returned dialog is not Dlg2');
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
    Dlg2 := TIdSipDialog.Create(ID2, Self.LocalSequenceNo, Self.RemoteSequenceNo, Self.LocalUri, Self.RemoteUri, Self.RemoteTarget, false, Self.RouteSet);
    try
      Self.D.Add(Dlg);
      Self.D.Add(Dlg2);

      Check(Dlg.ID.IsEqualTo(Self.D.DialogAt(Self.ID.CallID, Self.ID.LocalTag, Self.ID.RemoteTag).ID),
            'Returned dialog is not Dlg');
      Check(Dlg2.ID.IsEqualTo(Self.D.DialogAt(ID2.CallID, ID2.LocalTag, ID2.RemoteTag).ID),
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
