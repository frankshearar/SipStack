unit TestIdSipAuthentication;

interface

uses
  IdSipAuthentication, IdSipMessage, TestFramework, TestFrameworkSip;

type
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

implementation

uses
  IdHashMessageDigest, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Messages)');
  Result.AddTest(TestTIdSipAuthenticator.Suite);
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

initialization
  RegisterTest('Authentication framework', Suite);
end.
