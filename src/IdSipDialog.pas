{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipDialog;

interface

uses
  Classes, Contnrs, IdSipDialogID, IdSipMessage, SyncObjs;

type
  TIdSipDialog = class;
  TIdSipDialogState = (sdsNotInitialized, sdsEarly, sdsConfirmed);
  TIdSipDialogEvent = procedure(Sender: TIdSipDialog) of object;

  // cf RFC 3261, section 12.1
  // Within this specification, only 2xx and 101-199 responses with a To tag,
  // where the request was INVITE, will establish a dialog.
  TIdSipDialog = class(TPersistent)
  private
    fCanBeEstablished:   Boolean;
    fID:                 TIdSipDialogID;
    fInitialRequest:     TIdSipRequest;
    fInitialResponse:     TIdSipResponse;
    fIsSecure:           Boolean;
    fLocalSequenceNo:    Cardinal;
    fLocalURI:           TIdSipURI;
    fOnEstablished:      TIdSipDialogEvent;
    fRemoteSequenceNo:   Cardinal;
    fRemoteTarget:       TIdSipURI;
    fRemoteURI:          TIdSipURI;
    fRouteSet:           TIdSipRoutePath;
    fState:              TIdSipDialogState;
    LocalSequenceNoLock: TCriticalSection;

    function  GetIsEarly: Boolean;
    procedure CreateInternal(Request: TIdSipRequest;
                             Response: TIdSipResponse;
                             DialogID: TIdSipDialogID;
                             LocalSequenceNo: Cardinal;
                             RemoteSequenceNo: Cardinal;
                             const LocalUri: String;
                             const RemoteUri: String;
                             const RemoteTarget: String;
                             IsSecure: Boolean;
                             RouteSet: TIdSipHeaderList);
    function  GetLocalSequenceNo: Cardinal;
    procedure SetCanBeEstablished(Value: Boolean);
    procedure SetIsEarly(Value: Boolean);
    procedure SetIsSecure(Value: Boolean);
  protected
    procedure DoOnEstablished;
    procedure SetLocalSequenceNo(Value: Cardinal);
    procedure SetRemoteSequenceNo(Value: Cardinal);
    procedure SetRemoteTarget(Value: TIdSipURI);
    procedure SetState(Value: TIdSipDialogState);

    property CanBeEstablished: Boolean        read fCanBeEstablished;
    property InitialRequest:   TIdSipRequest  read fInitialRequest;
    property InitialResponse:  TIdSipResponse read fInitialResponse;
  public
    class function CreateInboundDialog(Request: TIdSipRequest;
                                       Response: TIdSipResponse;
                                       UsingSecureTransport: Boolean): TIdSipDialog;
    class function CreateOutboundDialog(Request: TIdSipRequest;
                                        Response: TIdSipResponse;
                                        UsingSecureTransport: Boolean): TIdSipDialog;

    constructor Create(Request: TIdSipRequest;
                       Response: TIdSipResponse;
                       DialogID: TIdSipDialogID;
                       LocalSequenceNo: Cardinal;
                       RemoteSequenceNo: Cardinal;
                       LocalUri,
                       RemoteUri: TIdSipURI;
                       RemoteTarget: TIdSipURI;
                       IsSecure: Boolean;
                       RouteSet: TIdSipHeaderList); overload;
    constructor Create(Request: TIdSipRequest;
                       Response: TIdSipResponse;
                       DialogID: TIdSipDialogID;
                       LocalSequenceNo: Cardinal;
                       RemoteSequenceNo: Cardinal;
                       const LocalUri: String;
                       const RemoteUri: String;
                       const RemoteTarget: String;
                       IsSecure: Boolean;
                       RouteSet: TIdSipHeaderList); overload;
    constructor Create(Dialog: TIdSipDialog); overload;
    destructor  Destroy; override;

    function  Copy: TIdSipDialog;
    function  CreateAck: TIdSipRequest;
    function  CreateRequest: TIdSipRequest;
    procedure ReceiveRequest(Request: TIdSipRequest); virtual;
    procedure ReceiveResponse(Response: TIdSipResponse); virtual;
    function  IsNull: Boolean; virtual;
    function  IsOutOfOrder(Request: TIdSipRequest): Boolean;
    function  NextInitialSequenceNo: Cardinal;
    function  NextLocalSequenceNo: Cardinal;

    property ID:               TIdSipDialogID  read fID;
    property IsEarly:          Boolean         read GetIsEarly;
    property IsSecure:         Boolean         read fIsSecure;
    property LocalSequenceNo:  Cardinal        read GetLocalSequenceNo;
    property LocalURI:         TIdSipURI       read fLocalURI;
    property RemoteSequenceNo: Cardinal        read fRemoteSequenceNo;
    property RemoteTarget:     TIdSipURI       read fRemoteTarget write SetRemoteTarget;
    property RemoteURI:        TIdSipURI       read fRemoteURI;
    property RouteSet:         TIdSipRoutePath read fRouteSet;

    property OnEstablished: TIdSipDialogEvent read fOnEstablished write fOnEstablished;
  end;

  TIdSipNullDialog = class(TIdSipDialog)
  public
    function IsNull: Boolean; override;
  end;

  TIdSipDialogs = class(TObject)
  private
    List:    TObjectList;
    Lock:    TCriticalSection;
    NullDlg: TIdSipNullDialog;

    function GetItem(Index: Integer): TIdSipDialog;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(NewDialog: TIdSipDialog);
    function  Count: Integer;
    function  DialogAt(ID: TIdSipDialogID): TIdSipDialog; overload;
    function  DialogAt(const CallID: String;
                       const LocalTag: String;
                       const RemoteTag: String): TIdSipDialog; overload;

    property Items[Index: Integer]: TIdSipDialog read GetItem;
  end;

const
  ItemNotFoundIndex = -1;

implementation

uses
  IdSipConsts, IdRandom, SysUtils;

//******************************************************************************
//* TIdSipDialog                                                               *
//******************************************************************************
//* TIdSipDialog Public methods ************************************************

class function TIdSipDialog.CreateInboundDialog(Request: TIdSipRequest;
                                                Response: TIdSipResponse;
                                                UsingSecureTransport: Boolean): TIdSipDialog;
var
  ID: TIdSipDialogID;
begin
  try
    ID := TIdSipDialogID.Create(Response.CallID,
                                Response.ToHeader.Tag,
                                Request.From.Tag);
    try
      Result := TIdSipDialog.Create(Request,
                                    Response,
                                    ID,
                                    0,
                                    Request.CSeq.SequenceNo,
                                    Request.ToHeader.Address,
                                    Request.From.Address,
                                    Request.FirstContact.Address,
                                    UsingSecureTransport and (Request.HasSipsUri),
                                    Request.RecordRoute);
    finally
      ID.Free;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdSipDialog.CreateOutboundDialog(Request: TIdSipRequest;
                                                 Response: TIdSipResponse;
                                                 UsingSecureTransport: Boolean): TIdSipDialog;
var
  ID:       TIdSipDialogID;
  RouteSet: TIdSipRecordRoutePath;
begin
  ID := TIdSipDialogID.Create(Request.CallID,
                              Request.From.Tag,
                              Response.ToHeader.Tag);
  try
    RouteSet := TIdSipRecordRoutePath.Create;
    try
      RouteSet.AddInReverseOrder(Response.RecordRoute);

      Result := TIdSipDialog.Create(Request,
                                    Response,
                                    ID,
                                    Request.CSeq.SequenceNo,
                                    0,
                                    Request.From.Address,
                                    Request.ToHeader.Address,
                                    Response.FirstContact.Address,
                                    UsingSecureTransport and Request.FirstContact.HasSipsUri,
                                    RouteSet);
    finally
      RouteSet.Free;
    end;
  finally
    ID.Free;
  end;
end;

constructor TIdSipDialog.Create(Request: TIdSipRequest;
                                Response: TIdSipResponse;
                                DialogID: TIdSipDialogID;
                                LocalSequenceNo: Cardinal;
                                RemoteSequenceNo: Cardinal;
                                LocalUri,
                                RemoteUri: TIdSipURI;
                                RemoteTarget: TIdSipURI;
                                IsSecure: Boolean;
                                RouteSet: TIdSipHeaderList);
begin
  inherited Create;

  Self.CreateInternal(Request,
                      Response,
                      DialogID,
                      LocalSequenceNo,
                      RemoteSequenceNo,
                      LocalUri.URI,
                      RemoteUri.URI,
                      RemoteTarget.URI,
                      IsSecure,
                      RouteSet);
end;

constructor TIdSipDialog.Create(Request: TIdSipRequest;
                                Response: TIdSipResponse;
                                DialogID: TIdSipDialogID;
                                LocalSequenceNo: Cardinal;
                                RemoteSequenceNo: Cardinal;
                                const LocalUri: String;
                                const RemoteUri: String;
                                const RemoteTarget: String;
                                IsSecure: Boolean;
                                RouteSet: TIdSipHeaderList);
begin
  inherited Create;

  Self.CreateInternal(Request,
                      Response,
                      DialogID,
                      LocalSequenceNo,
                      RemoteSequenceNo,
                      LocalUri,
                      RemoteUri,
                      RemoteTarget,
                      IsSecure,
                      RouteSet);
end;

constructor TIdSipDialog.Create(Dialog: TIdSipDialog);
begin
  inherited Create;

  Self.CreateInternal(Dialog.InitialRequest,
                      Dialog.InitialResponse,
                      Dialog.ID,
                      Dialog.LocalSequenceNo,
                      Dialog.RemoteSequenceNo,
                      Dialog.LocalUri.URI,
                      Dialog.RemoteUri.URI,
                      Dialog.RemoteTarget.URI,
                      Dialog.IsSecure,
                      Dialog.RouteSet);
end;

destructor TIdSipDialog.Destroy;
begin
  Self.RouteSet.Free;
  Self.RemoteTarget.Free;
  Self.RemoteURI.Free;
  Self.LocalUri.Free;
  Self.ID.Free;
  Self.LocalSequenceNoLock.Free;
  Self.InitialResponse.Free;
  Self.InitialRequest.Free;

  inherited Destroy;
end;

function TIdSipDialog.Copy: TIdSipDialog;
begin
  Result := TIdSipDialog.Create(Self);
  // TODO: This is mildly evil! We're editing the private variable of Result,
  // even though we're nice enough to do so through a setter.
  Result.SetIsEarly(Self.IsEarly);
end;

function TIdSipDialog.CreateAck: TIdSipRequest;
var
  FirstRoute: TIdSipRouteHeader;
  Routes:     TIdSipRoutePath;
begin
  Result := TIdSipRequest.Create;

  Result.Method           := MethodAck;
  Result.ToHeader.Address := Self.RemoteURI;
  Result.ToHeader.Tag     := Self.ID.RemoteTag;
  Result.From.Address     := Self.LocalURI;
  Result.From.Tag         := Self.ID.LocalTag;
  Result.CallID           := Self.ID.CallID;

  Result.CSeq.Method     := Result.Method;
  Result.CSeq.SequenceNo := Self.LocalSequenceNo;

  Result.AddHeader(Self.InitialRequest.LastHop);
  Result.CopyHeaders(Self.InitialRequest,
                     AuthorizationHeader);
  Result.CopyHeaders(Self.InitialRequest,
                     ProxyAuthorizationHeader);

  // We create an ACK as an in-dialog request. Further, an ACK to a 2xx
  // _cannot_ match a transaction since the 2xx _terminated_ the transaction.
  // Therefore we use a completely new branch/transaction ID.
  Result.LastHop.Branch := GRandomNumber.NextSipUserAgentBranch;

  if (Self.InitialRequest.ContentLength > 0) then begin
    Result.ContentLength := Self.InitialRequest.ContentLength;
    Result.Body          := Self.InitialRequest.Body;
    Result.ContentType   := Self.InitialRequest.ContentType;

    if Self.InitialRequest.HasHeader(ContentDispositionHeader) then
      Result.ContentDisposition := Self.InitialRequest.ContentDisposition;
  end;

  if Self.RouteSet.IsEmpty then begin
    Result.RequestUri := Self.RemoteTarget;
  end
  else begin
    Self.RouteSet.First;
    FirstRoute := Self.RouteSet.CurrentRoute;

    if FirstRoute.IsLooseRoutable then begin
      Result.RequestUri := Self.RemoteTarget;

      Result.Route := Self.RouteSet;
    end
    else begin
      Result.RequestUri := FirstRoute.Address;
      // RFC 3261 section 12.2.1.1; 19.1.5
      Result.RequestUri.Headers.Clear;
      Result.RequestUri.RemoveParameter(MethodParam);

      // Yes, we skip the first route. We use the 1st entry as the
      // Request-URI, remember?
      Routes := Self.RouteSet.GetAllButFirst;
      try
        Result.Route := Routes;
        Result.Route.AddRoute(Self.RemoteURI);
      finally
        Routes.Free;
      end;
    end;
  end;
end;

function TIdSipDialog.CreateRequest: TIdSipRequest;
var
  FirstRoute: TIdSipRouteHeader;
  Routes:     TIdSipRoutePath;
begin
  Result := TIdSipRequest.Create;

  Result.ToHeader.Address := Self.RemoteURI;
  Result.ToHeader.Tag     := Self.ID.RemoteTag;
  Result.From.Address     := Self.LocalURI;
  Result.From.Tag         := Self.ID.LocalTag;
  Result.CallID           := Self.ID.CallID;

  Result.CSeq.SequenceNo := Self.NextLocalSequenceNo;

  if Self.RouteSet.IsEmpty then begin
    Result.RequestUri := Self.RemoteTarget;
  end
  else begin
    Self.RouteSet.First;
    FirstRoute := Self.RouteSet.CurrentRoute;

    if FirstRoute.IsLooseRoutable then begin
      Result.RequestUri := Self.RemoteTarget;

      Result.Route := Self.RouteSet;
    end
    else begin
      // Strict routing
      Result.RequestUri := FirstRoute.Address;
      // RFC 3261 section 12.2.1.1; 19.1.5
      Result.RequestUri.Headers.Clear;
      Result.RequestUri.RemoveParameter(MethodParam);

      // Yes, we skip the first route. We use the 1st entry as the
      // Request-URI, remember?
      Routes := Self.RouteSet.GetAllButFirst;
      try
        Result.Route := Routes;
        Result.Route.AddRoute(Self.RemoteURI);
      finally
        Routes.Free;
      end;
    end;
  end;
end;

procedure TIdSipDialog.ReceiveRequest(Request: TIdSipRequest);
begin
  if Request.IsInvite then
    Self.SetCanBeEstablished(true);
end;

procedure TIdSipDialog.ReceiveResponse(Response: TIdSipResponse);
begin
  if (Self.RemoteTarget.Uri = '') then
    Self.RemoteTarget := Response.FirstContact.Address;

  if (Self.RemoteSequenceNo = 0) then
    Self.SetRemoteSequenceNo(Response.CSeq.SequenceNo);

  if Response.IsFinal then begin
    Self.SetIsEarly(false);

    if Self.CanBeEstablished and (Response.StatusCode = SIPOK) then
      Self.DoOnEstablished;
  end;

  if Response.IsProvisional then
    Self.SetIsEarly(true);
end;

function TIdSipDialog.IsNull: Boolean;
begin
  Result := false;
end;

function TIdSipDialog.IsOutOfOrder(Request: TIdSipRequest): Boolean;
begin
  Result := (Self.RemoteSequenceNo > 0)
        and (Request.CSeq.SequenceNo < Self.RemoteSequenceNo);
end;

function TIdSipDialog.NextInitialSequenceNo: Cardinal;
begin
  repeat
    Result := GRandomNumber.NextCardinal($7FFFFFFF);
  until (Result > 0);
end;

function TIdSipDialog.NextLocalSequenceNo: Cardinal;
begin
  Self.LocalSequenceNoLock.Acquire;
  try
    if (Self.fLocalSequenceNo = 0) then
      Self.fLocalSequenceNo := Self.NextInitialSequenceNo
    else
      Inc(Self.fLocalSequenceNo);

    Result := Self.fLocalSequenceNo;
  finally
    Self.LocalSequenceNoLock.Release;
  end;
end;

//* TIdSipDialog Protected methods *********************************************

procedure TIdSipDialog.DoOnEstablished;
begin
  if Assigned(Self.OnEstablished) then
    Self.OnEstablished(Self);
end;

procedure TIdSipDialog.SetLocalSequenceNo(Value: Cardinal);
begin
  Self.LocalSequenceNoLock.Acquire;
  try
    Self.fLocalSequenceNo := Value;
  finally
  Self.LocalSequenceNoLock.Release;
  end;
end;

procedure TIdSipDialog.SetRemoteSequenceNo(Value: Cardinal);
begin
  Self.fRemoteSequenceNo := Value;
end;

procedure TIdSipDialog.SetRemoteTarget(Value: TIdSipURI);
begin
  Self.RemoteTarget.Uri := Value.Uri;
end;

procedure TIdSipDialog.SetState(Value: TIdSipDialogState);
begin
  Self.fState := Value;
end;

//* TIdSipDialog Private methods ***********************************************

procedure TIdSipDialog.CreateInternal(Request: TIdSipRequest;
                                      Response: TIdSipResponse;
                                      DialogID: TIdSipDialogID;
                                      LocalSequenceNo: Cardinal;
                                      RemoteSequenceNo: Cardinal;
                                      const LocalUri: String;
                                      const RemoteUri: String;
                                      const RemoteTarget: String;
                                      IsSecure: Boolean;
                                      RouteSet: TIdSipHeaderList);
begin
  Self.fInitialRequest  := TIdSipRequest.Create;
  Self.fInitialRequest.Assign(Request);

  Self.fInitialResponse := TIdSipResponse.Create;
  Self.fInitialResponse.Assign(Response);

  Self.LocalSequenceNoLock := TCriticalSection.Create;

  Self.fID := TIdSipDialogID.Create(DialogID);
  Self.SetLocalSequenceNo(LocalSequenceNo);
  Self.SetRemoteSequenceNo(RemoteSequenceNo);

  Self.fLocalUri := TIdSipURI.Create(LocalUri);
  Self.fRemoteUri := TIdSipURI.Create(RemoteUri);

  Self.fRemoteTarget := TIdSipURI.Create(RemoteTarget);

  Self.SetIsSecure(IsSecure);

  Self.fRouteSet := TIdSipRoutePath.Create;

  // We normalise the RouteSet to contain only Routes. This just makes our
  // lives a bit simpler.
  RouteSet.First;
  while RouteSet.HasNext do begin
    Self.RouteSet.Add(RouteHeader).Assign(RouteSet.CurrentHeader);
    RouteSet.Next;
  end;
end;

function TIdSipDialog.GetIsEarly: Boolean;
begin
  Result := Self.fState = sdsEarly;
end;

function TIdSipDialog.GetLocalSequenceNo: Cardinal;
begin
  Self.LocalSequenceNoLock.Acquire;
  try
    Result := Self.fLocalSequenceNo;
  finally
    Self.LocalSequenceNoLock.Release;
  end;
end;

procedure TIdSipDialog.SetCanBeEstablished(Value: Boolean);
begin
  Self.fCanBeEstablished := Value;
end;

procedure TIdSipDialog.SetIsEarly(Value: Boolean);
begin
  if Value then
    Self.SetState(sdsEarly)
  else
    Self.SetState(sdsConfirmed);
end;

procedure TIdSipDialog.SetIsSecure(Value: Boolean);
begin
  Self.fIsSecure := Value;
end;

//******************************************************************************
//* TIdSipNullDialog                                                           *
//******************************************************************************
//* TIdSipNullDialog Public methods ********************************************

function TIdSipNullDialog.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdSipDialogs                                                              *
//******************************************************************************
//* TIdSipDialogs Public methods ***********************************************

constructor TIdSipDialogs.Create;
begin
  inherited Create;

  Self.List    := TObjectList.Create(true);
  Self.Lock    := TCriticalSection.Create;
  Self.NullDlg := TIdSipNullDialog.Create;
end;

destructor TIdSipDialogs.Destroy;
begin
  Self.NullDlg.Free;
  Self.Lock.Free;
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSipDialogs.Add(NewDialog: TIdSipDialog);
var
  D: TIdSipDialog;
begin
  Self.Lock.Acquire;
  try
    D := TIdSipDialog.Create(NewDialog);
    try
      Self.List.Add(D);
    except
      if (Self.List.IndexOf(D) <> ItemNotFoundIndex) then
        Self.List.Remove(D)
      else
        D.Free;

      raise;
    end;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipDialogs.Count: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := Self.List.Count;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipDialogs.DialogAt(ID: TIdSipDialogID): TIdSipDialog;
var
  I: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := nil;
    I := 0;
    while (I < Self.List.Count) and not Assigned(Result) do
      if Self.Items[I].ID.Equals(ID) then
        Result := Self.Items[I]
      else
        Inc(I);

    if not Assigned(Result) then
      Result := Self.NullDlg;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipDialogs.DialogAt(const CallID: String;
                                const LocalTag: String;
                                const RemoteTag: String): TIdSipDialog;
var
  ID: TIdSipDialogID;
begin
  ID := TIdSipDialogID.Create(CallID, LocalTag, RemoteTag);
  try
    Result := Self.DialogAt(ID);
  finally
    ID.Free;
  end;
end;

//* TIdSipDialogs Private methods **********************************************

function TIdSipDialogs.GetItem(Index: Integer): TIdSipDialog;
begin
  Result := Self.List[Index] as TIdSipDialog;
end;

end.
