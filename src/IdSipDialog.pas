unit IdSipDialog;

interface

uses
  Contnrs, IdURI, IdSipDialogID, IdSipMessage, IdSipHeaders, SyncObjs;

type
  TIdSipDialog = class;
  TIdSipDialogState = (sdsNotInitialised, sdsEarly, sdsConfirmed);
  TIdSipDialogEvent = procedure(Sender: TIdSipDialog) of object;

  // cf RFC 3261, section 12.1
  // Within this specification, only 2xx and 101-199 responses with a To tag,
  // where the request was INVITE, will establish a dialog.
  TIdSipDialog = class(TObject)
  private
    fCanBeEstablished: Boolean;
    fID:               TIdSipDialogID;
    fInitialRequest:   TIdSipRequest;
    fIsSecure:         Boolean;
    fLocalSequenceNo:  Cardinal;
    fLocalURI:         TIdURI;
    fOnEstablished:    TIdSipDialogEvent;
    fRemoteSequenceNo: Cardinal;
    fRemoteTarget:     TIdURI;
    fRemoteURI:        TIdURI;
    fRouteSet:         TIdSipHeaders;
    fState:            TIdSipDialogState;

    function  GetIsEarly: Boolean;
    procedure CreateInternal(const DialogID: TIdSipDialogID;
                       const LocalSequenceNo,
                             RemoteSequenceNo: Cardinal;
                       const LocalUri,
                             RemoteUri,
                             RemoteTarget: String;
                       const IsSecure: Boolean;
                       const RouteSet: TIdSipHeaderList);
    procedure SetCanBeEstablished(const Value: Boolean);
    procedure SetIsEarly(const Value: Boolean);
    procedure SetIsSecure(const Value: Boolean);
  protected
    procedure DoOnEstablished;
    procedure IncLocalSequenceNo;
    procedure SetLocalSequenceNo(const Value: Cardinal);
    procedure SetRemoteSequenceNo(const Value: Cardinal);
    procedure SetRemoteTarget(const Value: TIdURI);
    procedure SetState(const Value: TIdSipDialogState);

    property CanBeEstablished: Boolean       read fCanBeEstablished;
    property InitialRequest:   TIdSipRequest read fInitialRequest;
  public
    constructor Create(const DialogID: TIdSipDialogID;
                       const LocalSequenceNo,
                             RemoteSequenceNo: Cardinal;
                       const LocalUri,
                             RemoteUri: TIdURI;
                       const RemoteTarget: TIdURI;
                       const IsSecure: Boolean;
                       const RouteSet: TIdSipHeaderList); overload;
    constructor Create(const DialogID: TIdSipDialogID;
                       const LocalSequenceNo,
                             RemoteSequenceNo: Cardinal;
                       const LocalUri,
                             RemoteUri,
                             RemoteTarget: String;
                       const IsSecure: Boolean;
                       const RouteSet: TIdSipHeaderList); overload;
    constructor Create(const Dialog: TIdSipDialog); overload;
    destructor  Destroy; override;

    function  CreateBye: TIdSipRequest;
    function  CreateCancel: TIdSipRequest;
    function  CreateRequest: TIdSipRequest; virtual;
    procedure HandleMessage(const Request: TIdSipRequest); overload; virtual;
    procedure HandleMessage(const Response: TIdSipResponse); overload; virtual;
    function  IsNull: Boolean; virtual;

    property ID:               TIdSipDialogID read fID;
    property IsEarly:          Boolean        read GetIsEarly;
    property IsSecure:         Boolean        read fIsSecure;
    property LocalSequenceNo:  Cardinal       read fLocalSequenceNo write SetLocalSequenceNo;
    property LocalURI:         TIdURI         read fLocalURI;
    property RemoteSequenceNo: Cardinal       read fRemoteSequenceNo write SetRemoteSequenceNo;
    property RemoteTarget:     TIdURI         read fRemoteTarget write SetRemoteTarget;
    property RemoteURI:        TIdURI         read fRemoteURI;
    property RouteSet:         TIdSipHeaders  read fRouteSet;

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

    function GetItem(const Index: Integer): TIdSipDialog;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(const NewDialog: TIdSipDialog);
    function  Count: Integer;
    function  DialogAt(const ID: TIdSipDialogID): TIdSipDialog; overload;
    function  DialogAt(const CallID, LocalTag, RemoteTag: String): TIdSipDialog; overload;

    property Items[const Index: Integer]: TIdSipDialog read GetItem;
  end;

implementation

uses
  IdSipConsts, IdSipRandom, SysUtils;

//******************************************************************************
//* TIdSipDialog                                                               *
//******************************************************************************
//* TIdSipDialog Public methods ************************************************

constructor TIdSipDialog.Create(const DialogID: TIdSipDialogID;
                                const LocalSequenceNo,
                                      RemoteSequenceNo: Cardinal;
                                const LocalUri,
                                      RemoteUri: TIdURI;
                                const RemoteTarget: TIdURI;
                                const IsSecure: Boolean;
                                const RouteSet: TIdSipHeaderList);
begin
  inherited Create;

  Self.CreateInternal(DialogID,
                      LocalSequenceNo,
                      RemoteSequenceNo,
                      LocalUri.GetFullURI,
                      RemoteUri.GetFullURI,
                      RemoteTarget.GetFullURI,
                      IsSecure,
                      RouteSet);
end;

constructor TIdSipDialog.Create(const DialogID: TIdSipDialogID;
                   const LocalSequenceNo,
                         RemoteSequenceNo: Cardinal;
                   const LocalUri,
                         RemoteUri,
                         RemoteTarget: String;
                   const IsSecure: Boolean;
                   const RouteSet: TIdSipHeaderList);
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

constructor TIdSipDialog.Create(const Dialog: TIdSipDialog);
begin
  inherited Create;

  Self.CreateInternal(Dialog.ID,
                      Dialog.LocalSequenceNo,
                      Dialog.RemoteSequenceNo,
                      Dialog.LocalUri.GetFullURI,
                      Dialog.RemoteUri.GetFullURI,
                      Dialog.RemoteTarget.GetFullURI,
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

  inherited Destroy;
end;

function TIdSipDialog.CreateBye: TIdSipRequest;
begin
  try
    Result := Self.CreateRequest;
    Result.Method := MethodBye;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipDialog.CreateCancel: TIdSipRequest;
begin
  try
    Result := Self.CreateRequest;
    Result.Method      := MethodCancel;
    Result.CSeq.Method := MethodInvite;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipDialog.CreateRequest: TIdSipRequest;
var
  FirstRoute: TIdSipRouteHeader;
  I:          Integer;
begin
  Result := TIdSipRequest.Create;
  try
    Result.ToHeader.Address := Self.RemoteURI;
    Result.ToHeader.Tag     := Self.ID.RemoteTag;
    Result.From.Address     := Self.LocalURI;
    Result.From.Tag         := Self.ID.LocalTag;
    Result.CallID           := Self.ID.CallID;

    Result.CSeq.SequenceNo := Self.LocalSequenceNo;
    Result.CSeq.Method     := Result.Method;

    Self.IncLocalSequenceNo;

    if (Self.RouteSet.IsEmpty) then begin
      Result.RequestUri := Self.RemoteTarget;
    end
    else begin
      FirstRoute := Self.RouteSet.Items[0] as TIdSipRouteHeader;

      if FirstRoute.IsLooseRoutable then begin
        Result.RequestUri := Self.RemoteTarget;

        for I := 0 to Self.RouteSet.Count - 1 do
          Result.AddHeader(RouteHeader).Assign(Self.RouteSet.Items[I]);
      end
      else begin
        Result.RequestUri := FirstRoute.Address;

        // Yes, from 1 to count - 1. We use the 1st entry as the Request-URI,
        // remember?
        // No, we can't just Assign() here because (a) we're not adding ALL
        // the headers, and (b) we're adding Route headers, and RouteSet
        // contains Record-Route headers.
        for I := 1 to Self.RouteSet.Count - 1 do begin
          Result.AddHeader(RouteHeader).Value := Self.RouteSet.Items[I].Value
        end;

        (Result.AddHeader(RouteHeader) as TIdSipRouteHeader).Address := Self.RemoteURI;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TIdSipDialog.HandleMessage(const Request: TIdSipRequest);
begin
  if Request.IsInvite then
    Self.SetCanBeEstablished(true);
end;

procedure TIdSipDialog.HandleMessage(const Response: TIdSipResponse);
begin
  if (Self.RemoteTarget.GetFullUri = '') then
    Self.RemoteTarget.URI := Response.FirstContact.Address.GetFullUri;

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

//* TIdSipDialog Protected methods *********************************************

procedure TIdSipDialog.DoOnEstablished;
begin
  if Assigned(Self.OnEstablished) then
    Self.OnEstablished(Self);
end;

procedure TIdSipDialog.IncLocalSequenceNo;
begin
  Self.SetLocalSequenceNo(Self.LocalSequenceNo + 1);
end;

procedure TIdSipDialog.SetLocalSequenceNo(const Value: Cardinal);
begin
  Self.fLocalSequenceNo := Value;
end;

procedure TIdSipDialog.SetRemoteSequenceNo(const Value: Cardinal);
begin
  Self.fRemoteSequenceNo := Value;
end;

procedure TIdSipDialog.SetRemoteTarget(const Value: TIdURI);
begin
  Self.RemoteTarget.URI := Value.GetFullURI;
end;

procedure TIdSipDialog.SetState(const Value: TIdSipDialogState);
begin
  Self.fState := Value;
end;

//* TIdSipDialog Private methods ***********************************************

procedure TIdSipDialog.CreateInternal(const DialogID: TIdSipDialogID;
                                      const LocalSequenceNo,
                                            RemoteSequenceNo: Cardinal;
                                      const LocalUri,
                                            RemoteUri,
                                            RemoteTarget: String;
                                      const IsSecure: Boolean;
                                      const RouteSet: TIdSipHeaderList);
begin
  fID := TIdSipDialogID.Create(DialogID);
  Self.SetLocalSequenceNo(LocalSequenceNo);
  Self.SetRemoteSequenceNo(RemoteSequenceNo);

  fLocalUri := TIdURI.Create(LocalUri);
  fRemoteUri := TIdURI.Create(RemoteUri);

  fRemoteTarget := TIdURI.Create(RemoteTarget);

  Self.SetIsSecure(IsSecure);

  fRouteSet := TIdSipHeaders.Create;
  fRouteSet.Add(RouteSet);
end;

function TIdSipDialog.GetIsEarly: Boolean;
begin
  Result := Self.fState = sdsEarly;
end;

procedure TIdSipDialog.SetCanBeEstablished(const Value: Boolean);
begin
  Self.fCanBeEstablished := Value;
end;

procedure TIdSipDialog.SetIsEarly(const Value: Boolean);
begin
  if Value then
    Self.SetState(sdsEarly)
  else
    Self.SetState(sdsConfirmed);
end;

procedure TIdSipDialog.SetIsSecure(const Value: Boolean);
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

procedure TIdSipDialogs.Add(const NewDialog: TIdSipDialog);
var
  D: TIdSipDialog;
begin
  Self.Lock.Acquire;
  try
    D := TIdSipDialog.Create(NewDialog);
    try
      Self.List.Add(D);
    except
      Self.List.Remove(D);
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

function TIdSipDialogs.DialogAt(const ID: TIdSipDialogID): TIdSipDialog;
var
  I: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := nil;
    I := 0;
    while (I < Self.List.Count) and not Assigned(Result) do
      if (Self.List[I] as TIdSipDialog).ID.IsEqualTo(ID) then
        Result := Self.List[I] as TIdSipDialog
      else
        Inc(I);

    if not Assigned(Result) then
      Result := Self.NullDlg;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipDialogs.DialogAt(const CallID, LocalTag, RemoteTag: String): TIdSipDialog;
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

function TIdSipDialogs.GetItem(const Index: Integer): TIdSipDialog;
begin
  Result := Self.List[Index] as TIdSipDialog;
end;

end.
