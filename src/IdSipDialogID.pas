unit IdSipDialogID;

interface

type
  TIdSipDialogID = class(TObject)
  private
    fCallID:    String;
    fLocalTag:  String;
    fRemoteTag: String;
  public
    constructor Create(const CallID: String;
                       const LocalTag: String;
                       const RemoteTag: String); overload;
    constructor Create(ID: TIdSipDialogID); overload;

    function Equals(ID: TIdSipDialogID): Boolean;

    property CallID:    String read fCallID;
    property LocalTag:  String read fLocalTag;
    property RemoteTag: String read fRemoteTag;
  end;

implementation

//******************************************************************************
//* TIdSipDialogID                                                             *
//******************************************************************************
//* TIdSipDialogID Public methods **********************************************

constructor TIdSipDialogID.Create(const CallID: String;
                                  const LocalTag: String;
                                  const RemoteTag: String);
begin
  inherited Create;

  fCallID    := CallID;
  fLocalTag  := LocalTag;
  fRemoteTag := RemoteTag;
end;

constructor TIdSipDialogID.Create(ID: TIdSipDialogID);
begin
  inherited Create;

  fCallID    := ID.CallID;
  fLocalTag  := ID.LocalTag;
  fRemoteTag := ID.RemoteTag;
end;

function TIdSipDialogID.Equals(ID: TIdSipDialogID): Boolean;
begin
  Result := (Self.CallID    = ID.CallID)
        and (Self.LocalTag  = ID.LocalTag)
        and (Self.RemoteTag = ID.RemoteTag);
end;

end.
