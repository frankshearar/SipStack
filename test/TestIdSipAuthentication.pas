{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipAuthentication;

interface

uses
  IdSipAuthentication, IdSipMessage, TestFramework, TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestHashFor;
  end;

  TestTIdSipAuthenticator = class(TTestCase)
  private
    Auth:   TIdSipAuthenticator;
    Invite: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddUser;
    procedure TestDigest;
    procedure TestDontAuthenticateNormalRequest;
  end;

  TestTIdRealmInfo = class(TTestCase)
  private
    Body:           String;
    Challenge:      TIdSipWWWAuthenticateHeader;
    Method:         String;
    Opaque:         String;
    ProxyChallenge: TIdSipProxyAuthenticateHeader;
    RealmInfo:      TIdRealmInfo;

    procedure AddOpaqueToChallenge(Challenge: TIdSipAuthenticateHeader);
    procedure AddQopToChallenge(Challenge: TIdSipAuthenticateHeader);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAuthorization;
    procedure TestCreateAuthorizationHeaderType;
    procedure TestCreateProxyAuthorization;
    procedure TestMultipleAuthenticationAffectsNonceCount;
    procedure TestOpaque;
    procedure TestQopAuth;
  end;

  TestTIdKeyRing = class(TTestCase)
  private
    Ring: TIdKeyRing;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddKeyAndFind;
  end;

implementation

uses
  IdHashMessageDigest, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipAuthenication tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipAuthenticator.Suite);
  Result.AddTest(TestTIdRealmInfo.Suite);
  Result.AddTest(TestTIdKeyRing.Suite);
end;

//*******************************************************************************
//* TestFunctions                                                               *
//*******************************************************************************
//* TestFunctions Published methods *********************************************

procedure TestFunctions.TestHashFor;
begin
  Check(@MD5 = Pointer(HashFor('')),    'Empty string - assume MD5');
  Check(@MD5 = Pointer(HashFor('md5')), 'md5');
  Check(@MD5 = Pointer(HashFor('MD5')), 'MD5');
end;

//*******************************************************************************
//* TestTIdSipAuthenticator                                                     *
//*******************************************************************************
//* TestTIdSipAuthenticator Public methods **************************************

procedure TestTIdSipAuthenticator.SetUp;
begin
  inherited SetUp;

  Self.Auth   := TIdSipAuthenticator.Create;
  Self.Invite := TIdSipTestResources.CreateBasicRequest;
end;

procedure TestTIdSipAuthenticator.TearDown;
begin
  Self.Invite.Free;
  Self.Auth.Free;

  inherited TearDown;
end;

//* TestTIdSipAuthenticator Published methods ***********************************

procedure TestTIdSipAuthenticator.TestAddUser;
var
  Digest:   String;
  Realm:    String;
  Username: String;
begin
  Digest   := 'Baz';
  Realm    := 'Bar';
  Username := 'Foo';

  Self.Auth.AddUser(Username, Realm, Digest);
  CheckEquals(1, Self.Auth.Usercount, 'User not added - count');
  CheckEquals(Digest,
              Self.Auth.DigestFor(Username, Realm),
              'User not added - digest');

  Digest := 'Quaax';
  Self.Auth.AddUser(Username, Realm, Digest);
  CheckEquals(1,
              Self.Auth.Usercount,
              'User info not updated - count');
  CheckEquals(Digest,
              Self.Auth.DigestFor(Username, Realm),
              'User info not updated - digest');

  Username := 'Foobar';
  Self.Auth.AddUser(Username, Realm, Digest);
  CheckEquals(1,
              Self.Auth.Usercount,
              'New user info not added');
end;

procedure TestTIdSipAuthenticator.TestDigest;
var
  Digest: String;
  Md5:    TIdHashMessageDigest5;
begin
  Md5 := TIdHashMessageDigest5.Create;
  try
    Digest := Lowercase(Md5.AsHex(Md5.HashValue('Foo:Bar:Baz')));

    CheckEquals(Digest, Self.Auth.Digest('Foo', 'Bar', 'Baz'), 'Digest');
  finally
    Md5.Free;
  end;
end;

procedure TestTIdSipAuthenticator.TestDontAuthenticateNormalRequest;
begin
  // This INVITE has no Authorization or Proxy-Authorization header
  Check(not Self.Auth.Authenticate(Self.Invite),
        'No Authorization or Proxy-Authorization header');
end;

//*******************************************************************************
//* TestTIdRealmInfo                                                            *
//*******************************************************************************
//* TestTIdRealmInfo Public methods *********************************************

procedure TestTIdRealmInfo.SetUp;
begin
  inherited SetUp;

  Self.Challenge      := TIdSipWWWAuthenticateHeader.Create;
  Self.ProxyChallenge := TIdSipProxyAuthenticateHeader.Create;

  Self.Challenge.Value      := 'Digest realm="193.116.120.160",nonce="bfa807909eb7d5b960d7b23de1dc620ed82f40b5"';
  Self.ProxyChallenge.Value := Self.Challenge.Value;

  Self.RealmInfo := TIdRealmInfo.Create;
  Self.RealmInfo.DigestUri := 'sip:franks@localhost';
  Self.RealmInfo.Nonce     := Self.Challenge.Nonce;
  Self.RealmInfo.Password  := 'f00L';
  Self.RealmInfo.Realm     := Self.Challenge.Realm;
  Self.RealmInfo.Username  := 'wintermute';

  Self.Body   := '';
  Self.Method := MethodInvite;
  Self.Opaque := 'decafbadcafebabe';
end;

procedure TestTIdRealmInfo.TearDown;
begin
  Self.RealmInfo.Free;
  Self.Challenge.Free;
  Self.ProxyChallenge.Free;

  inherited TearDown;
end;

//* TestTIdRealmInfo Private methods *******************************************

procedure TestTIdRealmInfo.AddOpaqueToChallenge(Challenge: TIdSipAuthenticateHeader);
begin
  Challenge.Opaque := Self.Opaque;
end;

procedure TestTIdRealmInfo.AddQopToChallenge(Challenge: TIdSipAuthenticateHeader);
begin
  Challenge.Nonce := 'bfa807909eb7d5b960d7b23de1dc620ed82f40b5';
  Challenge.Qop   := QopAuth;
end;

//* TestTIdRealmInfo Published methods *****************************************

procedure TestTIdRealmInfo.TestCreateAuthorization;
var
  A1:     String;
  A2:     String;
  Auth:   TIdSipAuthorizationHeader;
begin
  Auth := Self.RealmInfo.CreateAuthorization(Self.ProxyChallenge,
                                             Self.Method,
                                             Self.Body);
  try
    CheckEquals(Self.RealmInfo.Nonce,
                Auth.Nonce,
                'Nonce');

    CheckEquals(Self.RealmInfo.Realm,
                Auth.Realm,
                'Realm');

    CheckEquals(Self.RealmInfo.DigestUri,
                Auth.DigestUri,
                'URI');

    CheckEquals(Self.RealmInfo.Username,
                Auth.Username,
                'Username');

    // Algorithm from RFC 2617, sections 3.2.2.1, 3.2.2.2
    A1 := Auth.Username + ':' + Auth.Realm + ':' + Self.RealmInfo.Password;
    A2 := Method + ':' + Auth.DigestUri;

    CheckEquals(KD(MD5(A1),
                   Auth.Nonce + ':' + MD5(A2),
                   MD5),
                Auth.Response,
                'Response');
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestCreateAuthorizationHeaderType;
var
  Auth: TIdSipAuthorizationHeader;
begin
  Auth := Self.RealmInfo.CreateAuthorization(Self.Challenge,
                                             Self.Method,
                                             Self.Body);
  try
    CheckEquals(AuthorizationHeader,
                Auth.Name,
                'Wrong header for ' + Self.Challenge.Name);
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestCreateProxyAuthorization;
var
  Auth: TIdSipAuthorizationHeader;
begin
  Auth := Self.RealmInfo.CreateAuthorization(Self.ProxyChallenge,
                                             Self.Method,
                                             Self.Body);
  try
    CheckEquals(ProxyAuthorizationHeader,
                Auth.Name,
                'Wrong header for ' + Self.ProxyChallenge.Name);
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestMultipleAuthenticationAffectsNonceCount;
var
  Auth: TIdSipAuthorizationHeader;
  I:    Integer;
begin
  Self.AddQopToChallenge(Self.ProxyChallenge);

  for I := 1 to 5 do begin
    Auth := Self.RealmInfo.CreateAuthorization(Self.ProxyChallenge,
                                               Self.Method,
                                               Self.Body);
    try
      CheckEquals(I,
                  Auth.NonceCount,
                  IntToStr(I) + 'th challenge: NonceCount');
    finally
      Auth.Free;
    end;
  end;
end;

procedure TestTIdRealmInfo.TestOpaque;
var
  Auth: TIdSipAuthorizationHeader;
begin
  Self.AddOpaqueToChallenge(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.ProxyChallenge,
                                             Self.Method,
                                             Self.Body);
  try
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestQopAuth;
var
  A1:   String;
  A2:   String;
  Auth: TIdSipAuthorizationHeader;
begin
  Self.AddQopToChallenge(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.ProxyChallenge,
                                             Self.Method,
                                             Self.Body);
  try
    Check(Auth.CNonce <> '',
          'Missing CNonce (Client Nonce)');
    Check(Auth.NonceCount > 0,
          'Insane Nonce Count');

    A1 := Auth.Username + ':' + Auth.Realm + ':' + Self.RealmInfo.Password;
    A2 := Self.Method + ':' + Self.RealmInfo.DigestUri + ':' + MD5(Self.Body);

    CheckEquals(KD(MD5(A1),
                   Auth.Nonce + ':'
                 + Auth.NC + ':'
                 + Auth.CNonce + ':'
                 + Auth.Qop + ':'
                 + MD5(A2),
                   MD5),
                Auth.Response,
                'Response');
  finally
    Auth.Free;
  end;
end;

//*******************************************************************************
//* TestTIdKeyRing                                                              *
//*******************************************************************************
//* TestTIdKeyRing Public methods ***********************************************

procedure TestTIdKeyRing.SetUp;
begin
  inherited SetUp;

  Self.Ring := TIdKeyRing.Create;
end;

procedure TestTIdKeyRing.TearDown;
begin
  Self.Ring.Free;

  inherited TearDown;
end;

//* TestTIdKeyRing Published methods ********************************************

procedure TestTIdKeyRing.TestAddKeyAndFind;
var
  Challenge: TIdSipWWWAuthenticateHeader;
  DigestUri: String;
  Info:      TIdRealmInfo;
begin
  DigestUri := 'sip:wintermust@tessier-ashpool.co.luna';

  Challenge := TIdSipWWWAuthenticateHeader.Create;
  try
    Challenge.Value := 'Digest realm="tessier-ashpool.co.luna",nonce="bfa807909eb7d5b960d7b23de1dc620ed82f40b5"';
    Self.Ring.AddKey(Challenge, DigestUri);

    Info := Self.Ring.Find(Challenge.Realm, DigestUri);
    CheckNotNull(Info,
                 'No credentials found');
    CheckEquals(DigestUri,
                Info.DigestUri,
                'DigestUri');
    CheckEquals(Challenge.Nonce,
                Info.Nonce,
                'Nonce');
    CheckEquals(0,
                Info.NonceCount,
                'NonceCount');
    CheckEquals(Challenge.Realm,
                Info.Realm,
                'Realm');
  finally
    Challenge.Free;
  end;
end;

initialization
  RegisterTest('Authentication framework', Suite);
end.
