unit TestIdURI;

interface

uses
  IdURI, TestFramework;

type
  TestTIdURI = class(TTestCase)
  private
    AllOptions: TIdURIOptionalFieldsSet;
  public
    procedure SetUp; override;
  published
    procedure TestFakeSchemeUri;
    procedure TestFtpUri;
    procedure TestHttpUri;
    procedure TestIPv4;
    procedure TestMalformedAuthority;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdURI unit tests');
  Result.AddTest(TestTIdURI.Suite);
end;

//******************************************************************************
//* TestTIdURI                                                                 *
//******************************************************************************
//* TestTIdURI Public methods **************************************************

procedure TestTIdURI.SetUp;
begin
  AllOptions := [Low(TIdURIOptionalFields)..High(TIdURIOptionalFields)];
end;

//* TestTIdURI Published methods ***********************************************

procedure TestTIdURI.TestFakeSchemeUri;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('fake+s-cheme.://www.hello.org');
  try
    CheckEquals('',                               URI.Bookmark,                    'Bookmark');
    CheckEquals('',                               URI.Document,                    'Document');
    CheckEquals('www.hello.org',                  URI.Host,                        'Host');
    CheckEquals('',                               URI.Params,                      'Params');
    CheckEquals('',                               URI.Password,                    'Password');
    CheckEquals('/',                              URI.Path,                        'Path');
    CheckEquals('',                               URI.Port,                        'Port');
    CheckEquals('fake+s-cheme.',                  URI.Protocol,                    'Protocol');
    CheckEquals('fake+s-cheme.://www.hello.org/', URI.URI,                         'URI incorrect');
    CheckEquals('fake+s-cheme.://www.hello.org/', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('',                               URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestFtpUri;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('ftp://god:dog@www.inferno.org:666/dante');
  try
    CheckEquals('',                                        URI.Bookmark,                    'Bookmark');
    CheckEquals('dante',                                   URI.Document,                    'Document');
    CheckEquals('www.inferno.org',                         URI.Host,                        'Host');
    CheckEquals('',                                        URI.Params,                      'Params');
    CheckEquals('dog',                                     URI.Password,                    'Password');
    CheckEquals('/',                                       URI.Path,                        'Path');
    CheckEquals('666',                                     URI.Port,                        'Port');
    CheckEquals('ftp',                                     URI.Protocol,                    'Protocol');
    CheckEquals('ftp://www.inferno.org:666/dante',         URI.URI,                         'URI');
    CheckEquals('ftp://god:dog@www.inferno.org:666/dante', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('god',                                     URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestHttpUri;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://my.server.com');
  try
    CheckEquals('',                      URI.Bookmark,                    'Bookmark');
    CheckEquals('',                      URI.Document,                    'Document');
    CheckEquals('my.server.com',         URI.Host,                        'Host');
    CheckEquals('',                      URI.Params,                      'Params');
    CheckEquals('',                      URI.Password,                    'Password');
    CheckEquals('/',                     URI.Path,                        'Path');
    CheckEquals('',                      URI.Port,                        'Port');
    CheckEquals('http',                  URI.Protocol,                    'Protocol');
    CheckEquals('http://my.server.com/', URI.URI,                         'URI incorrect');
    CheckEquals('http://my.server.com/', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('',                      URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestIPv4;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://192.168.0.1');
  try
    CheckEquals('',                    URI.Bookmark,                    'Bookmark');
    CheckEquals('',                    URI.Document,                    'Document');
    CheckEquals('192.168.0.1',         URI.Host,                        'Host');
    CheckEquals('',                    URI.Params,                      'Params');
    CheckEquals('',                    URI.Password,                    'Password');
    CheckEquals('/',                   URI.Path,                        'Path');
    CheckEquals('',                    URI.Port,                        'Port');
    CheckEquals('http',                URI.Protocol,                    'Protocol');
    CheckEquals('http://192.168.0.1/', URI.URI,                         'URI incorrect');
    CheckEquals('http://192.168.0.1/', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('',                    URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestMalformedAuthority;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://my:server.com');
  try
    CheckEquals('',                      URI.Bookmark,                    'Bookmark');
    CheckEquals('',                      URI.Document,                    'Document');
    CheckEquals('my:server.com',         URI.Host,                        'Host');
    CheckEquals('',                      URI.Params,                      'Params');
    CheckEquals('',                      URI.Password,                    'Password');
    CheckEquals('/',                     URI.Path,                        'Path');
    CheckEquals('',                      URI.Port,                        'Port');
    CheckEquals('http',                  URI.Protocol,                    'Protocol');
    // this should fail completely as ":" is illegal within an authority.
    // instead it's just interpreted as having an authority of "//my"
    CheckEquals('http://my:server.com/', URI.URI,                         'URI incorrect');
    CheckEquals('http://my:server.com/', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('',                      URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

initialization
  RegisterTest('Indy 9_0_14 TIdURI tests', Suite);
end.
