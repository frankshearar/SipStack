unit IdSipDialog;

interface

uses
  IdURI, IdSipMessage;

type
  TIdSipDialogState = (sdsNotInitialised, sdsEarly, sdsConfirmed);

  // Within this specification, only 2xx and 101-199 responses with a To tag,
  // where the request was INVITE, will establish a dialog.
  TIdSipDialogID = class(TObject)
  private
    fCallID:    String;
    fLocalTag:  String;
    fRemoteTag: String;
  public
    constructor Create(const CallID, LocalTag, RemoteTag: String);

    property CallID:    String read fCallID;
    property LocalTag:  String read fLocalTag;
    property RemoteTag: String read fRemoteTag;
  end;

  TIdSipDialog = class(TObject)
  private
    fID:               TIdSipDialogID;
    fInitialRequest:   TIdSipRequest;
    fIsSecure:         Boolean;
    fLocalSequenceNo:  Cardinal;
    fLocalURI:         TIdURI;
    fRemoteSequenceNo: Cardinal;
    fRemoteTarget:     TIdURI;
    fRemoteURI:        TIdURI;
    fRouteSet:         TIdSipHeaders;
    fState:            TIdSipDialogState;

    procedure SetIsSecure(const Value: Boolean);
  protected
    procedure InitialiseNewRequest(const R: TIdSipRequest); virtual;
    procedure SetLocalSequenceNo(const Value: Cardinal);
    procedure SetRemoteSequenceNo(const Value: Cardinal);
    procedure SetState(const Value: TIdSipDialogState);

    property InitialRequest: TIdSipRequest read fInitialRequest;
  public
    constructor Create(const Request: TIdSipRequest; const SentOverTLS: Boolean); virtual;
    destructor  Destroy; override;

    function  CreateRequest: TIdSipRequest;
    procedure HandleMessage(const Request: TIdSipRequest); overload; virtual;
    procedure HandleMessage(const Response: TIdSipResponse); overload; virtual;

    property ID:               TIdSipDialogID read fID;
    property IsSecure:         Boolean        read fIsSecure write fIsSecure;
    property LocalSequenceNo:  Cardinal       read fLocalSequenceNo write SetLocalSequenceNo;
    property LocalURI:         TIdURI         read fLocalURI;
    property RemoteSequenceNo: Cardinal       read fRemoteSequenceNo write SetRemoteSequenceNo;
    property RemoteTarget:     TIdURI         read fRemoteTarget;
    property RemoteURI:        TIdURI         read fRemoteURI;
    property RouteSet:         TIdSipHeaders  read fRouteSet;
  end;

  //   When a UAC sends a request that can establish a dialog (such as an
  //   INVITE) it MUST provide a SIP or SIPS URI with global scope (i.e.,
  //   the same SIP URI can be used in messages outside this dialog) in the
  //   Contact header field of the request.  If the request has a Request-
  //   URI or a topmost Route header field value with a SIPS URI, the
  //   Contact header field MUST contain a SIPS URI.
  //
  // So when do we check this?
  TIdSipUACDialog = class(TIdSipDialog)
  protected
    procedure InitialiseNewRequest(const R: TIdSipRequest); override;
  public
    constructor Create(const Request: TIdSipRequest; const SentOverTLS: Boolean); override;

    procedure HandleMessage(const Response: TIdSipResponse); overload; override;
  end;

  //   The UAS MUST add a Contact header field to
  //   the response.  The Contact header field contains an address where the
  //   UAS would like to be contacted for subsequent requests in the dialog
  //   (which includes the ACK for a 2xx response in the case of an INVITE).
  //   Generally, the host portion of this URI is the IP address or FQDN of
  //   the host.  The URI provided in the Contact header field MUST be a SIP
  //   or SIPS URI.  If the request that initiated the dialog contained a
  //   SIPS URI in the Request-URI or in the top Record-Route header field
  //   value, if there was any, or the Contact header field if there was no
  //   Record-Route header field, the Contact header field in the response
  //   MUST be a SIPS URI.
  TIdSipUASDialog = class(TIdSipDialog)
  private
    function  GetIsEarly: Boolean;
    procedure SetIsEarly(const Value: Boolean);
  public
    constructor Create(const Request: TIdSipRequest; const SentOverTLS: Boolean); override;

    procedure HandleMessage(const Response: TIdSipResponse); overload; override;

    property IsEarly: Boolean read GetIsEarly;
  end;

implementation

uses
  IdSipParser, IdSipRandom, SysUtils;

//******************************************************************************
//* TIdSipDialogID                                                             *
//******************************************************************************
//* TIdSipDialogID Public methods **********************************************

constructor TIdSipDialogID.Create(const CallID, LocalTag, RemoteTag: String);
begin
  inherited Create;

  fCallID    := CallID;
  fLocalTag  := LocalTag;
  fRemoteTag := RemoteTag;
end;

//******************************************************************************
//* TIdSipDialog                                                               *
//******************************************************************************
//* TIdSipDialog Public methods ************************************************

constructor TIdSipDialog.Create(const Request: TIdSipRequest; const SentOverTLS: Boolean);
begin
  inherited Create;

  Self.fInitialRequest := Request;

  Self.SetIsSecure(Request.HasSipsUri and SentOverTLS);
  Self.SetState(sdsNotInitialised);
end;

destructor TIdSipDialog.Destroy;
begin
  Self.RouteSet.Free;
  Self.RemoteURI.Free;
  Self.RemoteTarget.Free;
  Self.LocalUri.Free;
  Self.ID.Free;

  inherited Destroy;
end;

function TIdSipDialog.CreateRequest: TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Self.InitialiseNewRequest(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TIdSipDialog.HandleMessage(const Request: TIdSipRequest);
begin
end;

procedure TIdSipDialog.HandleMessage(const Response: TIdSipResponse);
begin
end;

//* TIdSipDialog Protected methods *********************************************

procedure TIdSipDialog.InitialiseNewRequest(const R: TIdSipRequest);
begin
end;

procedure TIdSipDialog.SetLocalSequenceNo(const Value: Cardinal);
begin
  Self.fLocalSequenceNo := Value;
end;

procedure TIdSipDialog.SetRemoteSequenceNo(const Value: Cardinal);
begin
  Self.fRemoteSequenceNo := Value;
end;

procedure TIdSipDialog.SetState(const Value: TIdSipDialogState);
begin
  Self.fState := Value;
end;

//* TIdSipDialog Private methods ***********************************************

procedure TIdSipDialog.SetIsSecure(const Value: Boolean);
begin
  Self.fIsSecure := Value;
end;

//******************************************************************************
//* TIdSipUACDialog                                                            *
//******************************************************************************
//* TIdSipUACDialog Public methods *********************************************

constructor TIdSipUACDialog.Create(const Request: TIdSipRequest; const SentOverTLS: Boolean);
var
  RouteFilter: TIdSipHeadersFilter;
begin
  inherited Create(Request, SentOverTLS);

  fID := TIdSipDialogID.Create(Request.CallID,
                               Request.From.Tag,
                               Request.ToHeader.Tag);

  Self.LocalSequenceNo  := Request.CSeq.SequenceNo;

  fLocalURI := TIdURI.Create(Request.From.Address.GetFullURI);

  Self.RemoteSequenceNo := 0;

  Self.fRemoteTarget := TIdURI.Create('');

  fRemoteURI := TIdURI.Create(Request.ToHeader.Address.GetFullURI);

  fRouteSet := TIdSipHeaders.Create;

  RouteFilter := TIdSipHeadersFilter.Create(Request.Headers, RecordRouteHeader);
  try
    Self.RouteSet.AddInReverseOrder(RouteFilter);
  finally
    RouteFilter.Free;
  end;
end;

procedure TIdSipUACDialog.HandleMessage(const Response: TIdSipResponse);
begin
  if (Self.RemoteTarget.GetFullUri = '') then
    Self.RemoteTarget.URI := (Response.Headers[ContactHeaderFull] as TIdSipContactHeader).Address.GetFullUri;

  if (Self.RemoteSequenceNo = 0) then
    Self.SetRemoteSequenceNo(Response.CSeq.SequenceNo);
end;

//* TIdSipUACDialog Protected methods ******************************************

procedure TIdSipUACDialog.InitialiseNewRequest(const R: TIdSipRequest);
var
  FirstRoute: TIdSipRouteHeader;
  I:          Integer;
begin
  R.ToHeader.Address := Self.RemoteURI;
  R.ToHeader.Tag     := Self.ID.RemoteTag;
  R.From.Address     := Self.LocalURI;
  R.From.Tag         := Self.ID.LocalTag;
  R.CallID           := Self.ID.CallID;

  R.CSeq.Method     := Self.InitialRequest.Method;
  R.CSeq.SequenceNo := TIdSipRandomNumber.Next;

  if (Self.RouteSet.IsEmpty) then begin
    R.RequestUri := Self.RemoteTarget.GetFullURI;
  end
  else begin
    FirstRoute := Self.RouteSet.Items[0] as TIdSipRouteHeader;

    if FirstRoute.HasLr then begin
      R.RequestUri := Self.RemoteTarget.GetFullURI;

      for I := 0 to Self.RouteSet.Count - 1 do
        R.Headers.Add(RouteHeader).Assign(Self.RouteSet.Items[I]);
    end
    else begin
      R.RequestUri := (FirstRoute).Address.GetFullUri;

      // Yes, from 1 to count - 1. We use the 1st entry as the Request-URI,
      // remember?
      for I := 1 to Self.RouteSet.Count - 1 do begin
        R.Headers.Add(RouteHeader).Assign(Self.RouteSet.Items[I]);
      end;

      R.Headers.Add(RouteHeader).Value := '<' + Self.RemoteURI.GetFullURI + '>';
    end;
  end;
end;

//******************************************************************************
//* TIdSipUASDialog                                                            *
//******************************************************************************
//* TIdSipUASDialog Public methods *********************************************

constructor TIdSipUASDialog.Create(const Request: TIdSipRequest; const SentOverTLS: Boolean);
var
  RouteFilter: TIdSipHeadersFilter;
begin
  inherited Create(Request, SentOverTLS);

  fID := TIdSipDialogID.Create(Request.CallID,
                               Request.ToHeader.Tag,
                               Request.From.Tag);

  Self.LocalSequenceNo  := 0;

  fLocalURI := TIdURI.Create(Request.ToHeader.Address.GetFullURI);

  Self.RemoteSequenceNo := Request.CSeq.SequenceNo;

  Self.fRemoteTarget := TIdURI.Create((Request.Headers[ContactHeaderFull] as TIdSipContactHeader).Address.GetFullURI);

  fRemoteURI := TIdURI.Create(Request.From.Address.GetFullURI);

  fRouteSet := TIdSipHeaders.Create;

  RouteFilter := TIdSipHeadersFilter.Create(Request.Headers, RecordRouteHeader);
  try
    Self.RouteSet.Add(RouteFilter);
  finally
    RouteFilter.Free;
  end;
end;

procedure TIdSipUASDialog.HandleMessage(const Response: TIdSipResponse);
begin
  inherited HandleMessage(Response);

  if Response.IsFinal then 
    Self.SetIsEarly(false);

  if Response.IsProvisional then
    Self.SetIsEarly(true);
end;

//* TIdSipUASDialog Private methods ********************************************

function TIdSipUASDialog.GetIsEarly: Boolean;
begin
  Result := Self.fState = sdsEarly;
end;

procedure TIdSipUASDialog.SetIsEarly(const Value: Boolean);
begin
  if Value then
    Self.SetState(sdsEarly)
  else
    Self.SetState(sdsConfirmed);
end;

end.
