{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipAuthentication;

interface

uses
  Contnrs, IdHashMessageDigest, IdSipMessage, SysUtils;

type
  // I provide a means for Transaction User components (i.e., User Agent
  // cores, Proxy cores, Registrar cores and the like) to authenticate a
  // request. I do this by something supplying me with a list of
  // username+realms and digests. I thus never see the unencrypted passwords.
  // I also supply a means to create the digests.

  TIdUserInfo = class(TObject)
  private
    fDigest:   String;
    fRealm:    String;
    fUsername: String;
  public
    property Digest:   String read fDigest write fDigest;
    property Realm:    String read fRealm write fRealm;
    property Username: String read fUsername write fUsername;
  end;

  TIdSipAbstractAuthenticator = class(TObject)
  private
    Coder:    TIdHashMessageDigest5;
    UserInfo: TObjectList;

    function HasUser(const Username: String;
                     const Realm: String): Boolean;
    function IndexOf(const Username: String;
                     const Realm: String): Integer;
    function InfoAt(Index: Integer): TIdUserInfo;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddUser(const Username: String;
                      const Realm: String;
                      const Digest: String); virtual;
    function  Authenticate(Request: TIdSipRequest): Boolean; virtual;
    function  DigestFor(const Username: String;
                        const Realm: String): String; virtual;
    function  Digest(const Username: String;
                     const Realm: String;
                     const Password: String): String;
    function  Usercount: Integer;
  end;

  TIdSipAuthenticator = class(TIdSipAbstractAuthenticator)
  end;

  EAuthenticate = class(Exception);
  EAuthenticateClass = class of EAuthenticate;

  TIdSipMockAuthenticator = class(TIdSipAuthenticator)
  private
    fAuthenticateAllRequests: Boolean;
    fFailWith:                EAuthenticateClass;
  public
    function Authenticate(Request: TIdSipRequest): Boolean; override;

    property AuthenticateAllRequests: Boolean            read fAuthenticateAllRequests write fAuthenticateAllRequests;
    property FailWith:                EAuthenticateClass read fFailWith write fFailWith;
  end;

type
  TIdHashFunction = function(const S: String): String;

function HashFor(const AlgorithmName: String): TIdHashFunction;
function KD(const Secret, Data: String; HashFunc: TIdHashFunction): String;
function MD5(const S: String): String;

implementation

uses
  Classes, IdSipConsts;

//*******************************************************************************
//* Unit functions & procedures                                                 *
//*******************************************************************************
//* Unit Public functions & procedures ******************************************

function HashFor(const AlgorithmName: String): TIdHashFunction;
begin
       if (AlgorithmName = '') then
    Result := MD5
  else if IsEqual(AlgorithmName, MD5Name) then
    Result := MD5
  else if IsEqual(AlgorithmName, MD5SessionName) then
    Result := MD5
  else
    Result := nil;
end;

function KD(const Secret, Data: String; HashFunc: TIdHashFunction): String;
begin
  Result := HashFunc(Trim(Secret) + ':' + Trim(Data));
end;

function MD5(const S: String): String;
var
  MD5: TIdHashMessageDigest5;
begin
  MD5 := TIdHashMessageDigest5.Create;
  try
    Result := Lowercase(MD5.AsHex(MD5.HashValue(S)));
  finally
    MD5.Free;
  end;
end;

//*******************************************************************************
//* TIdSipAbstractAuthenticator                                                 *
//*******************************************************************************
//* TIdSipAbstractAuthenticator Public methods **********************************

constructor TIdSipAbstractAuthenticator.Create;
begin
  inherited Create;

  Self.Coder    := TIdHashMessageDigest5.Create;
  Self.UserInfo := TObjectList.Create;
end;

destructor TIdSipAbstractAuthenticator.Destroy;
begin
  Self.UserInfo.Free;
  Self.Coder.Free;

  inherited Destroy;
end;

procedure TIdSipAbstractAuthenticator.AddUser(const Username: String;
                                              const Realm: String;
                                              const Digest: String);
var
  NewInfo: TIdUserInfo;
begin
  if Self.HasUser(Username, Realm) then begin
    Self.InfoAt(Self.IndexOf(Username, Realm)).Digest := Digest;
  end
  else begin
    NewInfo := TIdUserInfo.Create;
    try
      NewInfo.Username := Username;
      NewInfo.Realm    := Realm;
      NewInfo.Digest   := Digest;

      Self.UserInfo.Add(NewInfo);
    except
      if (Self.UserInfo.IndexOf(NewInfo) <> -1) then
        Self.UserInfo.Remove(NewInfo)
      else
        NewInfo.Free;
    end;
  end;
end;

function TIdSipAbstractAuthenticator.Authenticate(Request: TIdSipRequest): Boolean;
var
  Auth: TIdSipAuthorizationHeader;
begin
{
     In this document the string obtained by applying the digest
     algorithm to the data "data" with secret "secret" will be denoted
     by KD(secret, data), and the string obtained by applying the
     checksum algorithm to the data "data" will be denoted H(data). The
     notation unq(X) means the value of the quoted-string X without the
     surrounding quotes.

     For the "MD5" and "MD5-sess" algorithms

         H(data) = MD5(data)

     and

         KD(secret, data) = H(concat(secret, ":", data))

     i.e., the digest is the MD5 of the secret concatenated with a colon
     concatenated with the data. The "MD5-sess" algorithm is intended to
     allow efficient 3rd party authentication servers; for the
     difference in usage, see the description in section 3.2.2.2.
}

  if Request.HasAuthorization then begin
    Auth := Request.FirstAuthorization;
    Result := Auth.Response = Self.DigestFor(Auth.Username, Auth.Realm);
  end
  else if Request.HasProxyAuthorization then begin
    Result := false;
  end
  else
    Result := false;

  // This has to be clever enough to support both Proxy-Auth and WWW-Auth
end;

function TIdSipAbstractAuthenticator.Digest(const Username: String;
                                            const Realm: String;
                                            const Password: String): String;
begin
  Result := Self.Coder.AsHex(Self.Coder.HashValue(Username + ':' +
                                                  Realm + ':' +
                                                  Password));
  Result := Lowercase(Result);                                                
end;

function TIdSipAbstractAuthenticator.Usercount: Integer;
begin
  Result := Self.UserInfo.Count;
end;

function TIdSipAbstractAuthenticator.DigestFor(const Username: String;
                                               const Realm: String): String;
begin
  if Self.HasUser(Username, Realm) then
    Result := Self.InfoAt(Self.IndexOf(Username, Realm)).Digest
  else
    Result := '';
end;

//* TIdSipAbstractAuthenticator Private methods ********************************

function TIdSipAbstractAuthenticator.HasUser(const Username: String;
                                             const Realm: String): Boolean;
begin
  Result := Self.IndexOf(Username, Realm) <> -1;
end;

function TIdSipAbstractAuthenticator.IndexOf(const Username: String;
                                             const Realm: String): Integer;
begin
  Result := 0;
  while (Result < Self.UserInfo.Count)
    and (Self.InfoAt(Result).Username <> Username)
    and (Self.InfoAt(Result).Realm <> Realm) do Inc(Result);

  if (Result = Self.UserInfo.Count) then
    Result := -1;
end;

function TIdSipAbstractAuthenticator.InfoAt(Index: Integer): TIdUserInfo;
begin
  Result := Self.UserInfo[Index] as TIdUserInfo;
end;

//*******************************************************************************
//* TIdSipMockAuthenticator                                                     *
//*******************************************************************************
//* TIdSipMockAuthenticator Public methods **************************************

function TIdSipMockAuthenticator.Authenticate(Request: TIdSipRequest): Boolean;
begin
  if (Self.FailWith <> nil) then
    raise Self.FailWith.Create('TIdSipMockAuthenticator.Authenticate');

  if Self.AuthenticateAllRequests then Result := true
  else
    Result := inherited Authenticate(Request);
end;

end.
