unit IdSipDialog;

interface

uses
  Contnrs, IdSipDialogID, IdSipMessage, SyncObjs;

type
  TIdSipDialog = class;
  TIdSipDialogState = (sdsNotInitialized, sdsEarly, sdsConfirmed);
  TIdSipDialogEvent = procedure(Sender: TIdSipDialog) of object;

  // cf RFC 3261, section 12.1
  // Within this specification, only 2xx and 101-199 responses with a To tag,
  // where the request was INVITE, will establish a dialog.
  TIdSipDialog = class(TObject)
  private
    fCanBeEstablished:   Boolean;
    fID:                 TIdSipDialogID;
    fInitialRequest:     TIdSipRequest;
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
    procedure CreateInternal(DialogID: TIdSipDialogID;
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

    property CanBeEstablished: Boolean       read fCanBeEstablished;
    property InitialRequest:   TIdSipRequest read fInitialRequest;
  public
    constructor Create(DialogID: TIdSipDialogID;
                       LocalSequenceNo: Cardinal;
                       RemoteSequenceNo: Cardinal;
                       LocalUri,
                       RemoteUri: TIdSipURI;
                       RemoteTarget: TIdSipURI;
                       IsSecure: Boolean;
                       RouteSet: TIdSipHeaderList); overload;
    constructor Create(DialogID: TIdSipDialogID;
                       LocalSequenceNo: Cardinal;
                       RemoteSequenceNo: Cardinal;
                       const LocalUri: String;
                       const RemoteUri: String;
                       const RemoteTarget: String;
                       IsSecure: Boolean;
                       RouteSet: TIdSipHeaderList); overload;
    constructor Create(Dialog: TIdSipDialog); overload;
    destructor  Destroy; override;

    function  CreateRequest: TIdSipRequest;
    procedure HandleMessage(Request: TIdSipRequest); overload; virtual;
    procedure HandleMessage(Response: TIdSipResponse); overload; virtual;
    function  IsNull: Boolean; virtual;
    function  IsOutOfOrder(Request: TIdSipRequest): Boolean;
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

implementation

uses
  IdSipConsts, IdRandom, SysUtils;

//******************************************************************************
//* TIdSipDialog                                                               *
//******************************************************************************
//* TIdSipDialog Public methods ************************************************

constructor TIdSipDialog.Create(DialogID: TIdSipDialogID;
                                LocalSequenceNo: Cardinal;
                                RemoteSequenceNo: Cardinal;
                                LocalUri,
                                RemoteUri: TIdSipURI;
                                RemoteTarget: TIdSipURI;
                                IsSecure: Boolean;
                                RouteSet: TIdSipHeaderList);
begin
  inherited Create;

  Self.CreateInternal(DialogID,
                      LocalSequenceNo,
                      RemoteSequenceNo,
                      LocalUri.URI,
                      RemoteUri.URI,
                      RemoteTarget.URI,
                      IsSecure,
                      RouteSet);
end;

constructor TIdSipDialog.Create(DialogID: TIdSipDialogID;
                                LocalSequenceNo: Cardinal;
                                RemoteSequenceNo: Cardinal;
                                const LocalUri: String;
                                const RemoteUri: String;
                                const RemoteTarget: String;
                                IsSecure: Boolean;
                                RouteSet: TIdSipHeaderList);
begin
  inherited Create;

  Self.CreateInternal(DialogID,
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

  Self.CreateInternal(Dialog.ID,
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

  inherited Destroy;
end;

function TIdSipDialog.CreateRequest: TIdSipRequest;
var
  FirstRoute: TIdSipRouteHeader;
  Routes:     TIdSipRoutePath;
begin
  Result := TIdSipRequest.Create;

  Result.MaxForwards      := Result.DefaultMaxForwards;
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

procedure TIdSipDialog.HandleMessage(Request: TIdSipRequest);
begin
  if Request.IsInvite then
    Self.SetCanBeEstablished(true);
end;

procedure TIdSipDialog.HandleMessage(Response: TIdSipResponse);
begin
  if (Self.RemoteTarget.Uri = '') then
    Self.RemoteTarget.URI := Response.FirstContact.Address.URI;

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

function TIdSipDialog.NextLocalSequenceNo: Cardinal;
begin
  Self.LocalSequenceNoLock.Acquire;
  try
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
  Self.RemoteTarget.URI := Value.URI;
end;

procedure TIdSipDialog.SetState(Value: TIdSipDialogState);
begin
  Self.fState := Value;
end;

//* TIdSipDialog Private methods ***********************************************

procedure TIdSipDialog.CreateInternal(DialogID: TIdSipDialogID;
                                      LocalSequenceNo: Cardinal;
                                      RemoteSequenceNo: Cardinal;
                                      const LocalUri: String;
                                      const RemoteUri: String;
                                      const RemoteTarget: String;
                                      IsSecure: Boolean;
                                      RouteSet: TIdSipHeaderList);
begin
  Self.LocalSequenceNoLock := TCriticalSection.Create;

  fID := TIdSipDialogID.Create(DialogID);
  Self.SetLocalSequenceNo(LocalSequenceNo);
  Self.SetRemoteSequenceNo(RemoteSequenceNo);

  fLocalUri := TIdSipURI.Create(LocalUri);
  fRemoteUri := TIdSipURI.Create(RemoteUri);

  fRemoteTarget := TIdSipURI.Create(RemoteTarget);

  Self.SetIsSecure(IsSecure);

  fRouteSet := TIdSipRoutePath.Create;

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

  Self.List := TObjectList.Create(true);
  Self.Lock := TCriticalSection.Create;
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
      if (Self.List.IndexOf(D) <> -1) then
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
      if Self.Items[I].ID.IsEqualTo(ID) then
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
