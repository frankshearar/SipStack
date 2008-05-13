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
  private
    Auth:     TIdSipAuthorizationHeader;
    Body:     String;
    Method:   String;
    Password: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMD5A1;
    procedure TestMD5SessionA1;
    procedure TestQopAuthA2;
    procedure TestQopAuthIntA2;
    procedure TestQopAuthRequestDigest;
    procedure TestQopAuthIntRequestDigest;
    procedure TestQopNotSpecifiedRequestDigest;
    procedure TestPreregisteredA1Fors;
    procedure TestPreregisteredA2Fors;
    procedure TestPreregisteredHashFors;
    procedure TestPreregisteredRequestDigestFors;
    procedure TestRegisterAlgorithm;
    procedure TestRegisterAndUnregisterAuthenticationPolicy;
    procedure TestRegisterHash;
    procedure TestRegisterQop;
    procedure TestRequestDigestFor;
    procedure TestReregisterHash;
    procedure TestUnregisterAlgorithm;
    procedure TestUnregisterHash;
    procedure TestUnregisterQop;
    procedure TestUnregisterRequestDigest;
    procedure TestUseUnregisteredHash;
  end;

  TAuthenticatorTestCase = class(TTestCase)
  private
    Auth:   TIdSipAbstractAuthenticator;
    Invite: TIdSipRequest;
  protected
    function CreateAuthenticator: TIdSipAbstractAuthenticator; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddUser;
    procedure TestAuthenticateAsUserAgent;
    procedure TestCreateChallengeResponse;
    procedure TestCreateChallengeResponseAsProxy;
    procedure TestCreateChallengeResponseAsUserAgent;
    procedure TestDontAuthenticateNormalRequest; virtual;
  end;

  TestTIdSipBasicAuthenticator = class(TAuthenticatorTestCase)
  protected
    function CreateAuthenticator: TIdSipAbstractAuthenticator; override;
  end;

  TestTIdSipNullAuthenticator = class(TAuthenticatorTestCase)
  protected
    function CreateAuthenticator: TIdSipAbstractAuthenticator; override;
  published
    procedure TestAuthenticate;
    procedure TestDontAuthenticateNormalRequest; override;
  end;

  TestTIdRealmInfo = class(TTestCase)
  private
    Body:           String;
    Challenge:      TIdSipWWWAuthenticateHeader;
    Method:         String;
    Opaque:         String;
    Password:       String;
    ProxyChallenge: TIdSipProxyAuthenticateHeader;
    RealmInfo:      TIdRealmInfo;
    Response:       TIdSipResponse;

    procedure AddOpaqueToChallenge(Challenge: TIdSipAuthenticateHeader);
    procedure AddQopToChallenge(Challenge: TIdSipAuthenticateHeader;
                                const Qop: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAlgorithmMD5;
    procedure TestCreateAuthorization;
    procedure TestCreateAuthorizationHeaderType;
    procedure TestCreateProxyAuthorization;
    procedure TestMultipleAuthenticationAffectsNonceCount;
    procedure TestAuthenticationInfoSetsNextNonce;
    procedure TestOpaque;
    procedure TestQopAuth;
    procedure TestQopAuthInt;
  end;

  TestTIdKeyRing = class(TTestCase)
  private
    Ring: TIdKeyRing;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddKeyAndFind;
    procedure TestAddKeyTwice;
  end;

implementation

uses
  IdHashMessageDigest, SysUtils;

type
  TFooAuthenticator = class(TIdSipAbstractAuthenticator);
  TBarAuthenticator = class(TIdSipAbstractAuthenticator);

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipAuthenication tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipBasicAuthenticator.Suite);
  Result.AddTest(TestTIdSipNullAuthenticator.Suite);
  Result.AddTest(TestTIdRealmInfo.Suite);
  Result.AddTest(TestTIdKeyRing.Suite);
end;

//*******************************************************************************
//* Unit Private functions & procedures                                         *
//*******************************************************************************

function Identity(const S: String): String;
begin
  Result := S;
end;

function NoQop(Auth: TIdSipAuthorizationHeader;
               const Method: String;
               const Body: String): String;
begin
  Result := '';
end;

function NullAlgorithm(Auth: TIdSipAuthorizationHeader;
                       const Password: String): String;
begin
  Result := '';
end;

function NullRequestDigest(const A1: String;
                           const A2: String;
                           Auth: TIdSipAuthorizationHeader): String;
begin
  Result := '';
end;

//*******************************************************************************
//* TestFunctions                                                               *
//*******************************************************************************
//* TestFunctions Public methods ************************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.Auth := TIdSipAuthorizationHeader.Create;
  Self.Auth.Username  := 'case';
  Self.Auth.DigestUri := 'sip:case@fried-neurons.org';
  Self.Auth.Nonce     := 'cafebabe';
  Self.Auth.CNonce    := 'decafbad';

  Self.Body     := 'Neuromancer';
  Self.Method   := MethodInvite;
  Self.Password := 'mycotoxin';
end;

procedure TestFunctions.TearDown;
begin
  Self.Auth.Free;

  inherited TearDown;
end;

//* TestFunctions Published methods *********************************************

procedure TestFunctions.TestMD5A1;
begin
  CheckEquals(Self.Auth.Username + ':' + Self.Auth.Realm + ':' + Self.Password,
              MD5A1(Self.Auth, Self.Password),
              MD5Name);
end;

procedure TestFunctions.TestMD5SessionA1;
begin
  CheckEquals(Self.Auth.Username + ':' + Self.Auth.Realm + ':' + Self.Password
            + ':' + Self.Auth.Nonce + ':' + Self.Auth.CNonce,
              MD5SessionA1(Self.Auth, Self.Password),
              MD5SessionName);
end;

procedure TestFunctions.TestPreregisteredA1Fors;
begin
  Check(@AlgorithmNotSpecifiedA1 = Pointer(A1For('')),                        'Empty string');
  Check(@MD5A1                   = Pointer(A1For(MD5Name)),                   MD5Name);
  Check(@MD5A1                   = Pointer(A1For(Uppercase(MD5Name))),        Uppercase(MD5Name));
  Check(@MD5A1                   = Pointer(A1For(Lowercase(MD5Name))),        Lowercase(MD5Name));
  Check(@MD5SessionA1            = Pointer(A1For(MD5SessionName)),            MD5SessionName);
  Check(@MD5SessionA1            = Pointer(A1For(Uppercase(MD5SessionName))), Uppercase(MD5SessionName));
  Check(@MD5SessionA1            = Pointer(A1For(Lowercase(MD5SessionName))), Lowercase(MD5SessionName));
  Check(nil                      = Pointer(A1For('unknown')),                 'unknown');
end;

procedure TestFunctions.TestPreregisteredA2Fors;
begin
  Check(@QopNotSpecifiedA2 = Pointer(A2For('')),                    'Empty string');
  Check(@QopAuthA2         = Pointer(A2For(QopAuth)),               QopAuth);
  Check(@QopAuthA2         = Pointer(A2For(Uppercase(QopAuth))),    Uppercase(QopAuth));
  Check(@QopAuthIntA2      = Pointer(A2For(QopAuthInt)),            QopAuthInt);
  Check(@QopAuthIntA2      = Pointer(A2For(Uppercase(QopAuthInt))), Uppercase(QopAuthInt));
  Check(nil                = Pointer(A2For('unknown')),             'unknown');
end;

procedure TestFunctions.TestPreregisteredHashFors;
begin
  Check(@MD5 = Pointer(HashFor('')),                 'Empty string - assume MD5');
  Check(@MD5 = Pointer(HashFor(MD5Name)),            MD5Name);
  Check(@MD5 = Pointer(HashFor(Uppercase(MD5Name))), Uppercase(MD5Name));
  Check(@MD5 = Pointer(HashFor(Lowercase(MD5Name))), Lowercase(MD5Name));
  Check(nil  = Pointer(HashFor('unknown')),          'unknown');
end;

procedure TestFunctions.TestPreregisteredRequestDigestFors;
begin
  Check(@QopNotSpecifiedRequestDigest = Pointer(RequestDigestFor('')),
        'Empty string - assume auth');
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(QopAuth)),
        QopAuth);
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(Lowercase(QopAuth))),
        Lowercase(QopAuth));
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(Uppercase(QopAuth))),
        Uppercase(QopAuth));
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(QopAuthInt)),
        QopAuthInt);
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(Lowercase(QopAuthInt))),
        Lowercase(QopAuthInt));
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(Uppercase(QopAuthInt))),
        Uppercase(QopAuthInt));
end;

procedure TestFunctions.TestQopAuthA2;
begin
  CheckEquals(Self.Method + ':' + Self.Auth.DigestUri,
              QopAuthA2(Self.Auth, Self.Method, Self.Body),
              QopAuth);
end;

procedure TestFunctions.TestQopAuthIntA2;
var
  H: TIdHashFunction;
begin
  H := HashFor(Self.Auth.Algorithm);
  CheckEquals(Self.Method + ':' + Self.Auth.DigestUri + ':' + H(Self.Body),
              QopAuthIntA2(Self.Auth, Self.Method, Self.Body),
              QopAuthInt);
end;

procedure TestFunctions.TestQopAuthRequestDigest;
var
  A1: TIdAlgorithmFunction;
  A2: TIdQopFunction;
  H:  TIdHashFunction;
begin
  Self.Auth.Qop := QopAuth;

  H := HashFor(Self.Auth.Algorithm);
  A1 := A1For(Self.Auth.Algorithm);
  A2 := A2For(Self.Auth.Qop);

  CheckEquals(KD(H(A1(Self.Auth, Self.Password)),
                 Self.Auth.Nonce + ':'
               + Self.Auth.NC + ':'
               + Self.Auth.CNonce + ':'
               + Self.Auth.Qop + ':'
               + H(A2(Self.Auth, Self.Method, Self.Body)),
                H),
              QopAuthRequestDigest(A1(Self.Auth, Self.Password),
                                   A2(Self.Auth, Self.Method, Self.Body),
                                   Auth),
             QopAuth);
end;

procedure TestFunctions.TestQopAuthIntRequestDigest;
var
  A1: TIdAlgorithmFunction;
  A2: TIdQopFunction;
  H:  TIdHashFunction;
begin
  Self.Auth.Qop := QopAuthInt;

  H := HashFor(Self.Auth.Algorithm);
  A1 := A1For(Self.Auth.Algorithm);
  A2 := A2For(Self.Auth.Qop);

  CheckEquals(KD(H(A1(Self.Auth, Self.Password)),
                 Self.Auth.Nonce + ':'
               + Self.Auth.NC + ':'
               + Self.Auth.CNonce + ':'
               + Self.Auth.Qop + ':'
               + H(A2(Self.Auth, Self.Method, Self.Body)),
                H),
              QopAuthIntRequestDigest(A1(Self.Auth, Self.Password),
                                      A2(Self.Auth, Self.Method, Self.Body),
                                      Auth),
              QopAuthInt);
end;

procedure TestFunctions.TestQopNotSpecifiedRequestDigest;
var
  A1: TIdAlgorithmFunction;
  A2: TIdQopFunction;
  H:  TIdHashFunction;
begin
  Self.Auth.Qop := '';

  H := HashFor(Self.Auth.Algorithm);
  A1 := A1For(Self.Auth.Algorithm);
  A2 := A2For(Self.Auth.Qop);

  CheckEquals(KD(H(A1(Self.Auth, Self.Password)),
                 Self.Auth.Nonce + ':'
               + H(A2(Self.Auth, Self.Method, Self.Body)),
                 H),
              QopNotSpecifiedRequestDigest(A1(Self.Auth, Self.Password),
                                           A2(Self.Auth, Self.Method, Self.Body),
                                           Auth),
              'qop not specified');
end;

procedure TestFunctions.TestRegisterAlgorithm;
  function NullAlgorithm(Auth: TIdSipAuthorizationHeader;
                         const Password: String): String;
  begin
    Result := '';
  end;
const
  NullAlgorithmName = 'null';
begin
  Check(nil = Pointer(A1For(NullAlgorithmName)), NullAlgorithmName + 'already present');

  RegisterAlgorithm(NullAlgorithmName, @NullAlgorithm);
  Check(@NullAlgorithm = Pointer(A1For(NullAlgorithmName)),
        NullAlgorithmName + 'not registered');
end;

procedure TestFunctions.TestRegisterAndUnregisterAuthenticationPolicy;
const
  FooName = 'Foo';
begin
  CheckEquals(TIdSipNullAuthenticator, AuthenticationPolicyFor(FooName), 'Default authentication policy');

  RegisterAuthenticationPolicy(FooName, TFooAuthenticator);
  try
    CheckEquals(TFooAuthenticator, AuthenticationPolicyFor(FooName), 'Authentication policy not registered');

    RegisterAuthenticationPolicy(FooName, TBarAuthenticator);
    CheckEquals(TBarAuthenticator, AuthenticationPolicyFor(FooName), 'Authentication policy registration not overwritten');
  finally
    UnregisterAuthenticationPolicy(FooName);
  end;
  CheckEquals(TIdSipNullAuthenticator, AuthenticationPolicyFor(FooName), 'Authentication policy not unregistered');
end;

procedure TestFunctions.TestRegisterHash;
  function NullHash(const S: String): String;
  begin
    Result := '';
  end;
const
  NullHashName = 'null';
begin
  Check(nil = Pointer(HashFor(NullHashName)), NullHashName + 'already present');

  RegisterHash(NullHashName, @NullHash);
  Check(@NullHash = Pointer(HashFor(NullHashName)),
        NullHashName + 'not registered');
end;

procedure TestFunctions.TestRegisterQop;
  function NullQop(Auth: TIdSipAuthorizationHeader;
                   const Method: String;
                   const Body: String): String;
  begin
    Result := '';
  end;
const
  NullQopName = 'null';
begin
  Check(nil = Pointer(A2For(NullQopName)), NullQopName + 'already present');

  RegisterQop(NullQopName, @NullQop);
  Check(@NullQop = Pointer(A2For(NullQopName)),
        NullQopName + 'not registered');
end;

procedure TestFunctions.TestRequestDigestFor;
begin
  Check(@QopNotSpecifiedRequestDigest = Pointer(RequestDigestFor('')),
        'Empty string');
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(QopAuth)),
        QopAuth);
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(Uppercase(QopAuth))),
        Uppercase(QopAuth));
  Check(@QopAuthRequestDigest = Pointer(RequestDigestFor(Lowercase(QopAuth))),
        Lowercase(QopAuth));
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(QopAuthInt)),
        QopAuthInt);
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(Uppercase(QopAuthInt))),
        Uppercase(QopAuthInt));
  Check(@QopAuthIntRequestDigest = Pointer(RequestDigestFor(Lowercase(QopAuthInt))),
        Lowercase(QopAuthInt));
  Check(nil = Pointer(RequestDigestFor('unknown')), 'unknown');
end;

procedure TestFunctions.TestReregisterHash;
const
  NewHash = 'foo';
begin
  RegisterHash(NewHash, MD5);
  RegisterHash(NewHash, Identity);
  try
    Check(@Identity = Pointer(HashFor(NewHash)), 'Hash entry not replaced');
  finally
    UnregisterHash(NewHash);
  end;
end;

procedure TestFunctions.TestUnregisterAlgorithm;
const
  Null = 'null';
var
  Auth: TIdSipAuthorizationHeader;
begin
  Auth := TIdSipAuthorizationHeader.Create;
  try
    RegisterAlgorithm(Null, NullAlgorithm);
    UnregisterAlgorithm(Null);

    Check(nil = Pointer(A1For(Null)), 'Algorithm not unregistered');
  finally
    Auth.Free;
  end;
end;

procedure TestFunctions.TestUnregisterHash;
const
  IdentityHash = 'identity';
begin
  RegisterHash(IdentityHash, Identity);
  UnregisterHash(IdentityHash);

  Check(nil = Pointer(HashFor(IdentityHash)), 'Hash not unregistered');
end;

procedure TestFunctions.TestUnregisterQop;
const
  None = 'none';
begin
  RegisterQop(None, NoQop);
  UnregisterQop(None);

  Check(nil = Pointer(A2For(None)), 'Qop function not unregistered');
end;

procedure TestFunctions.TestUnregisterRequestDigest;
const
  None = 'none';
begin
  RegisterRequestDigest(None, NullRequestDigest);
  UnregisterRequestDigest(None);

  Check(nil = Pointer(RequestDigestFor(None)), 'Qop function not unregistered');
end;

procedure TestFunctions.TestUseUnregisteredHash;
begin
  Check(nil = Pointer(HashFor('foo')), 'HashFor returned something unexpected');
end;

//******************************************************************************
//* TAuthenticatorTestCase                                                     *
//******************************************************************************
//* TAuthenticatorTestCase Public methods **************************************

procedure TAuthenticatorTestCase.SetUp;
begin
  inherited SetUp;

  Self.Auth   := Self.CreateAuthenticator;
  Self.Invite := TIdSipTestResources.CreateBasicRequest;
end;

procedure TAuthenticatorTestCase.TearDown;
begin
  Self.Invite.Free;
  Self.Auth.Free;

  inherited TearDown;
end;

//* TAuthenticatorTestCase Protected methods ***********************************

function TAuthenticatorTestCase.CreateAuthenticator: TIdSipAbstractAuthenticator;
begin
  Result := nil;
  Fail(Self.ClassName + ' MUST override CreateAuthenticator');
end;

//* TAuthenticatorTestCase Published methods ***********************************

procedure TAuthenticatorTestCase.TestAddUser;
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

procedure TAuthenticatorTestCase.TestAuthenticateAsUserAgent;
begin
  Self.Auth.IsProxy := true;
  Fail('Not implemented');
end;

procedure TAuthenticatorTestCase.TestCreateChallengeResponse;
var
  Challenge: TIdSipResponse;
  Request:   TIdSipRequest;
begin
  Self.Auth.IsProxy := false;
  Self.Auth.Algorithm := MD5Name;
  Self.Auth.Qop       := QopAuthInt;
  Self.Auth.Realm     := 'tessier-ashpool.co.luna';

  Request := TIdSipTestResources.CreateBasicRequest;
  try
    Challenge := Self.Auth.CreateChallengeResponse(Request);
    try
      CheckEquals(SIPUnauthorized,
                  Challenge.StatusCode,
                  'Status-Code');
      Check(Challenge.HasWWWAuthenticate,
            'No WWW-Authenticate header');
      CheckEquals(Self.Auth.Algorithm,
                  Challenge.FirstWWWAuthenticate.Algorithm,
                  'Algorithm');
      CheckEquals(DigestAuthorizationScheme,
                  Challenge.FirstWWWAuthenticate.AuthorizationScheme,
                  'Authorization scheme');
      CheckNotEquals('',
                     Challenge.FirstWWWAuthenticate.Nonce,
                     'Nonce value');
      CheckEquals(Self.Auth.Qop,
                  Challenge.FirstWWWAuthenticate.Qop,
                  'Qop');
      CheckEquals(Self.Auth.Realm,
                  Challenge.FirstWWWAuthenticate.Realm,
                  'Realm');
    finally
      Challenge.Free;
    end;
  finally
    Request.Free;
  end;
end;

procedure TAuthenticatorTestCase.TestCreateChallengeResponseAsProxy;
var
  Challenge: TIdSipResponse;
  Request:   TIdSipRequest;
begin
  Self.Auth.IsProxy := true;

  Request := TIdSipTestResources.CreateBasicRequest;
  try
    Challenge := Self.Auth.CreateChallengeResponse(Request);
    try
      CheckEquals(SIPProxyAuthenticationRequired,
                  Challenge.StatusCode,
                  'Status-Code');
      Check(Challenge.HasProxyAuthenticate,
            'No Proxy-Authenticate header');

    finally
      Challenge.Free;
    end;
  finally
    Request.Free;
  end;
end;

procedure TAuthenticatorTestCase.TestCreateChallengeResponseAsUserAgent;
var
  Challenge: TIdSipResponse;
  Request:   TIdSipRequest;
begin
  // Proxies behave as User Agents in the context of SIP Events (see RFC 3265).

  Self.Auth.IsProxy := true;

  Request := TIdSipTestResources.CreateBasicRequest;
  try
    Challenge := Self.Auth.CreateChallengeResponseAsUserAgent(Request);
    try
      CheckEquals(SIPUnauthorized,
                  Challenge.StatusCode,
                  'Status-Code');
      Check(Challenge.HasWWWAuthenticate,
            'No WWW-Authenticate header');

    finally
      Challenge.Free;
    end;
  finally
    Request.Free;
  end;
end;

procedure TAuthenticatorTestCase.TestDontAuthenticateNormalRequest;
begin
  // This INVITE has no Authorization or Proxy-Authorization header
  Check(not Self.Auth.Authenticate(Self.Invite),
        'No Authorization or Proxy-Authorization header');
end;

//******************************************************************************
//* TestTIdSipBasicAuthenticator                                               *
//******************************************************************************
//* TestTIdSipBasicAuthenticator Protected methods *****************************

function TestTIdSipBasicAuthenticator.CreateAuthenticator: TIdSipAbstractAuthenticator;
begin
  Result :=TIdSipBasicAuthenticator.Create;
end;

//******************************************************************************
//* TestTIdSipNullAuthenticator                                                *
//******************************************************************************
//* TestTIdSipNullAuthenticator Protected methods ******************************

function TestTIdSipNullAuthenticator.CreateAuthenticator: TIdSipAbstractAuthenticator;
begin
  Result :=TIdSipNullAuthenticator.Create;
end;

//* TestTIdSipNullAuthenticator Published methods ******************************

procedure TestTIdSipNullAuthenticator.TestAuthenticate;
begin
  Self.Invite.AddHeader(AuthorizationHeader);
  Self.Invite.FirstAuthorization.Realm     := 'fried-neurons.org';
  Self.Invite.FirstAuthorization.Username  := 'case';
  Self.Invite.FirstAuthorization.DigestUri := 'sip:case@fried-neurons.org';
  Self.Invite.FirstAuthorization.Nonce     := 'cafebabe';
  Self.Invite.FirstAuthorization.CNonce    := 'decafbad';

  Check(Self.Auth.Authenticate(Self.Invite), 'Null authenticator: Authorization header');

  Self.Invite.FirstAuthorization.Realm := 'tessier-ashpool.co.luna';
  Check(Self.Auth.Authenticate(Self.Invite), 'Null authenticator: Authorization header with different realm');
end;

procedure TestTIdSipNullAuthenticator.TestDontAuthenticateNormalRequest;
begin
  // Null authenticator authenticates everything.

  Check(Self.Auth.Authenticate(Self.Invite), 'Null authenticator: No Authorization or Proxy-Authorization header');
end;

//******************************************************************************
//* TestTIdRealmInfo                                                           *
//******************************************************************************
//* TestTIdRealmInfo Public methods ********************************************

procedure TestTIdRealmInfo.SetUp;
begin
  inherited SetUp;

  Self.Challenge      := TIdSipWWWAuthenticateHeader.Create;
  Self.ProxyChallenge := TIdSipProxyAuthenticateHeader.Create;

  Self.Challenge.Value      := 'Digest realm="193.116.120.160",nonce="bfa807909eb7d5b960d7b23de1dc620ed82f40b5"';
  Self.ProxyChallenge.Value := Self.Challenge.Value;

  Self.Password  := 'f00L';

  Self.RealmInfo := TIdRealmInfo.Create;
  Self.RealmInfo.Digest    := Self.RealmInfo.Username
                            + ':' + Self.RealmInfo.Realm
                            + ':' + Self.Password;
  Self.RealmInfo.DigestUri := 'sip:franks@localhost';
  Self.RealmInfo.Nonce     := Self.Challenge.Nonce;
  Self.RealmInfo.Realm     := Self.Challenge.Realm;
  Self.RealmInfo.Username  := 'wintermute';

  Self.Response := TIdSipResponse.Create;

  Self.Body   := '';
  Self.Method := MethodInvite;
  Self.Opaque := 'decafbadcafebabe';
end;

procedure TestTIdRealmInfo.TearDown;
begin
  Self.Response.Free;
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

procedure TestTIdRealmInfo.AddQopToChallenge(Challenge: TIdSipAuthenticateHeader;
                                             const Qop: String);
begin
  Challenge.Nonce := 'bfa807909eb7d5b960d7b23de1dc620ed82f40b5';
  Challenge.Qop   := Qop;
end;

//* TestTIdRealmInfo Published methods *****************************************

procedure TestTIdRealmInfo.TestAlgorithmMD5;
var
  Auth: TIdSipAuthorizationHeader;
begin
  Self.ProxyChallenge.Algorithm := MD5Name;
  Self.Response.AddHeader(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
    CheckEquals(Self.ProxyChallenge.Algorithm,
                Auth.Algorithm,
                'Algorithm');
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestCreateAuthorization;
var
  A1:   String;
  A2:   String;
  Algo: TIdAlgorithmFunction;
  Auth: TIdSipAuthorizationHeader;
  Qop:  TIdQopFunction;
begin
  Self.Response.AddHeader(Self.ProxyChallenge);
  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
    CheckEquals(Self.ProxyChallenge.AuthorizationScheme,
                Auth.AuthorizationScheme,
                'Authorization scheme');

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

    CheckEquals(Self.ProxyChallenge.Qop,
                Auth.Qop,
                'Qop');

    Algo := A1For(Self.ProxyChallenge.Algorithm);
    Qop  := A2For(Self.ProxyChallenge.Qop);

    A1 := Algo(Auth, Self.Password);
    A2 := Qop(Auth, Self.Method, Self.Body);

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
  Self.Response.AddHeader(Self.Challenge);
  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
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
  Self.Response.AddHeader(Self.ProxyChallenge);
  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
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
  Self.AddQopToChallenge(Self.ProxyChallenge, QopAuth);
  Self.Response.AddHeader(Self.ProxyChallenge);

  for I := 1 to 5 do begin
    Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                               Self.Method,
                                               Self.Body,
                                               Self.Password);
    try
      CheckEquals(I,
                  Auth.NonceCount,
                  IntToStr(I) + 'th challenge: NonceCount');
    finally
      Auth.Free;
    end;
  end;
end;

procedure TestTIdRealmInfo.TestAuthenticationInfoSetsNextNonce;
const
  NextNonce = 'f00f00';
var
  Auth: TIdSipAuthorizationHeader;
begin
  // We add qop-auth so we can inspect NonceCount.
  Self.AddQopToChallenge(Self.Challenge,
                         QopAuth);

  Self.Response.AddHeader(AuthenticationInfoHeader);
  Self.Response.FirstAuthenticationInfo.NextNonce := NextNonce;

  Self.Response.AddHeader(Self.Challenge);
  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
   CheckEquals(NextNonce, Self.RealmInfo.Nonce, 'NextNonce not stored');
  finally
    Auth.Free;
  end;

  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
   CheckEquals(NextNonce, Auth.Nonce,      'NextNonce not used in next attempt');
   CheckEquals(1,         Auth.NonceCount, 'NonceCount not reset');
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestOpaque;
var
  Auth: TIdSipAuthorizationHeader;
begin
  Self.AddOpaqueToChallenge(Self.ProxyChallenge);
  Self.Response.AddHeader(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
  finally
    Auth.Free;
  end;
end;

procedure TestTIdRealmInfo.TestQopAuth;
var
  A1:   String;
  A2:   String;
  Algo: TIdAlgorithmFunction;
  Auth: TIdSipAuthorizationHeader;
  Qop:  TIdQopFunction;
begin
  Self.AddQopToChallenge(Self.ProxyChallenge, QopAuth);
  Self.Response.AddHeader(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
    CheckEquals(QopAuth,
                Auth.Qop,
                'Qop');

    Check(Auth.CNonce <> '',
          'Missing CNonce (Client Nonce)');
    Check(Auth.NonceCount > 0,
          'Insane Nonce Count');

    Algo := A1For(Self.ProxyChallenge.Algorithm);
    Qop  := A2For(Self.ProxyChallenge.Qop);

    A1 := Algo(Auth, Self.Password);
    A2 := Qop(Auth, Self.Method, Self.Body);

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

procedure TestTIdRealmInfo.TestQopAuthInt;
var
  A1:   String;
  A2:   String;
  Algo: TIdAlgorithmFunction;
  Auth: TIdSipAuthorizationHeader;
  Qop:  TIdQopFunction;
begin
  Self.AddQopToChallenge(Self.ProxyChallenge, QopAuthInt);
  Self.Response.AddHeader(Self.ProxyChallenge);

  Auth := Self.RealmInfo.CreateAuthorization(Self.Response,
                                             Self.Method,
                                             Self.Body,
                                             Self.Password);
  try
    CheckEquals(QopAuthInt,
                Auth.Qop,
                'Qop');

    Check(Auth.CNonce <> '',
          'Missing CNonce (Client Nonce)');
    Check(Auth.NonceCount > 0,
          'Insane Nonce Count');

    Algo := A1For(Self.ProxyChallenge.Algorithm);
    Qop  := A2For(Self.ProxyChallenge.Qop);

    A1 := Algo(Auth, Self.Password);
    A2 := Qop(Auth, Self.Method, Self.Body);

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

//******************************************************************************
//* TestTIdKeyRing                                                             *
//******************************************************************************
//* TestTIdKeyRing Public methods **********************************************

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

//* TestTIdKeyRing Published methods *******************************************

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
    Self.Ring.AddKey(Challenge, DigestUri, 'wintermute');

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

procedure TestTIdKeyRing.TestAddKeyTwice;
var
  Challenge: TIdSipWWWAuthenticateHeader;
  DigestUri: String;
begin
  DigestUri := 'sip:wintermust@tessier-ashpool.co.luna';

  Challenge := TIdSipWWWAuthenticateHeader.Create;
  try
    Challenge.Value := 'Digest realm="tessier-ashpool.co.luna",nonce="bfa807909eb7d5b960d7b23de1dc620ed82f40b5"';
    Self.Ring.AddKey(Challenge, DigestUri, 'wintermute');

    CheckEquals(1, Self.Ring.KeyCount, 'One AddKey');

    Self.Ring.AddKey(Challenge, DigestUri, 'wintermute');

    CheckEquals(1, Self.Ring.KeyCount, 'Two AddKeys');
  finally
    Challenge.Free;
  end;
end;

initialization
  RegisterTest('Authentication framework', Suite);
end.
