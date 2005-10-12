{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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

    function AsString: String;
    function Equals(ID: TIdSipDialogID): Boolean;

    property CallID:    String read fCallID;
    property LocalTag:  String read fLocalTag;
    property RemoteTag: String read fRemoteTag;
  end;

const
  DialogIdStringForm = '(dialog-id callid: %s localtag: %s remotetag: %s)';

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipDialogID                                                             *
//******************************************************************************
//* TIdSipDialogID Public methods **********************************************

constructor TIdSipDialogID.Create(const CallID: String;
                                  const LocalTag: String;
                                  const RemoteTag: String);
begin
  inherited Create;

  Self.fCallID    := CallID;
  Self.fLocalTag  := LocalTag;
  Self.fRemoteTag := RemoteTag;
end;

constructor TIdSipDialogID.Create(ID: TIdSipDialogID);
begin
  inherited Create;

  Self.fCallID    := ID.CallID;
  Self.fLocalTag  := ID.LocalTag;
  Self.fRemoteTag := ID.RemoteTag;
end;

function TIdSipDialogID.AsString: String;
begin
  Result := Format(DialogIdStringForm,
                   [Self.CallID, Self.LocalTag, Self.RemoteTag]);
end;

function TIdSipDialogID.Equals(ID: TIdSipDialogID): Boolean;
begin
  Result := (Self.CallID    = ID.CallID)
        and (Self.LocalTag  = ID.LocalTag)
        and (Self.RemoteTag = ID.RemoteTag);
end;

end.
