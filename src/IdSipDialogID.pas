{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipDialogID;

interface

uses
  Classes;

type
  TIdSipDialogID = class(TPersistent)
  private
    fCallID:    String;
    fLocalTag:  String;
    fRemoteTag: String;

    procedure Initialise(CallID, LocalTag, RemoteTag: String);
  public
    constructor Create; overload;
    constructor Create(const CallID: String;
                       const LocalTag: String;
                       const RemoteTag: String); overload;
    constructor Create(ID: TIdSipDialogID); overload;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  Equals(ID: TIdSipDialogID): Boolean;
    function  GetRemoteID: TIdSipDialogID;

    property CallID:    String read fCallID    write fCallID;
    property LocalTag:  String read fLocalTag  write fLocalTag;
    property RemoteTag: String read fRemoteTag write fRemoteTag;
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

constructor TIdSipDialogID.Create;
begin
  inherited Create;

  Self.Initialise('', '', '');
end;

constructor TIdSipDialogID.Create(const CallID: String;
                                  const LocalTag: String;
                                  const RemoteTag: String);
begin
  inherited Create;

  Self.Initialise(CallID, LocalTag, RemoteTag);
end;

constructor TIdSipDialogID.Create(ID: TIdSipDialogID);
begin
  inherited Create;

  Self.Initialise(ID.CallID, ID.LocalTag, ID.RemoteTag);
end;

procedure TIdSipDialogID.Assign(Src: TPersistent);
var
  Other: TIdSipDialogID;
begin
  // Foo.Assign(Foo) should do nothing.
  if (Self = Src) then Exit;

  if (Src is TIdSipDialogID) then begin
    Other := Src as TIdSipDialogID;

    Self.fCallID    := Other.CallID;
    Self.fLocalTag  := Other.LocalTag;
    Self.fRemoteTag := Other.RemoteTag;
  end;
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

function TIdSipDialogID.GetRemoteID: TIdSipDialogID;
begin
  Result := TIdSipDialogID.Create(Self.CallID, Self.RemoteTag, Self.LocalTag);
end;

//* TIdSipDialogID Private methods *********************************************

procedure TIdSipDialogID.Initialise(CallID, LocalTag, RemoteTag: String);
begin
  Self.fCallID    := CallID;
  Self.fLocalTag  := LocalTag;
  Self.fRemoteTag := RemoteTag;
end;

end.
