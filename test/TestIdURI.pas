unit TestIdURI;

interface

uses
  IdURI, IdHttp, TestFramework;

type
  TestTIdURI = class(TTestCase)
  private
    AllOptions: TIdURIOptionalFieldsSet;
  public
    procedure SetUp; override;
  published
    procedure TestFakeSchemeUri;
    procedure TestFtpUri;
    procedure TestIPv4;
    procedure TestMailTo;
//    procedure TestMalformedAuthority;
    procedure TestTest1;
    procedure TestTest2;
    procedure TestTest3;
    procedure TestTest4;
    procedure TestTest5;
    procedure TestTest6;
    procedure TestTest9;
    procedure TestTest10;
    procedure TestTest11;
    procedure TestTest12;
    procedure TestTest13;
    procedure TestTest14;
    procedure TestTest15;
    procedure TestTest16;
    procedure TestTest17;
    procedure TestTest18;
  end;

implementation

uses
  IdResourceStrings;

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
  URI := TIdURI.Create('fake+s-cheme.://www.hello.org/');
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

procedure TestTIdURI.TestIPv4;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://192.168.0.1/');
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

procedure TestTIdURI.TestMailTo;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('mailto:azatoth@outer.darkness');
  try
    CheckEquals('',                              URI.Bookmark,                    'Bookmark');
    CheckEquals('',                              URI.Document,                    'Document');
    CheckEquals('outer.darkness',                URI.Host,                        'Host');
    CheckEquals('',                              URI.Params,                      'Params');
    CheckEquals('',                              URI.Password,                    'Password');
    CheckEquals('',                              URI.Path,                        'Path');
    CheckEquals('',                              URI.Port,                        'Port');
    CheckEquals('mailto',                        URI.Protocol,                    'Protocol');
    CheckEquals('mailto:outer.darkness',         URI.URI,                         'URI incorrect');
    CheckEquals('mailto:azatoth@outer.darkness', URI.GetFullURI(Self.AllOptions), 'Full URI');
    CheckEquals('azatoth',                       URI.Username,                    'Username');
  finally
    URI.Free;
  end;
end;

{
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
}

procedure TestTIdURI.TestTest1;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com');
  try
    CheckEquals('http://www.nevrona.com/', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest2;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '';
    URI.Document := '';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '';
    URI.Bookmark := '';

    CheckEquals('http://www.nevrona.com', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params,   'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest3;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('ftp://www.nevrona.com/document');
  try
    CheckEquals('ftp://www.nevrona.com/document', URI.URI, 'URI');
    CheckEquals('ftp://www.nevrona.com/document', URI.GetFullURI, 'GetFullURI');
    CheckEquals('ftp', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest4;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('');
  try
    URI.Protocol := 'ftp';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/';
    URI.Document := 'document';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '';
    URI.Bookmark := '';

    CheckEquals('ftp://www.nevrona.com/document', URI.URI, 'URI');
    CheckEquals('ftp://www.nevrona.com/document', URI.GetFullURI, 'GetFullURI');
    CheckEquals('ftp', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest5;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/path/document');
  try
    CheckEquals('http://www.nevrona.com/path/document', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path/document', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest6;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/path/';
    URI.Document := 'document';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '';
    URI.Bookmark := '';

    CheckEquals('http://www.nevrona.com/path/document', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path/document', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest9;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/path/');
  try
    CheckEquals('http://www.nevrona.com/path/', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path/', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path/', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest10;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/path/');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/path/';
    URI.Document := '';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '';
    URI.Bookmark := '';

    CheckEquals('http://www.nevrona.com/path/', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path/', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path/', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest11;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/path1/path2/path3/path4/');
  try
    CheckEquals('http://www.nevrona.com/path1/path2/path3/path4/', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path1/path2/path3/path4/', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path1/path2/path3/path4/', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest12;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/path1/path2/path3/path4/');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/path1/path2/path3/path4/';
    URI.Document := '';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '';
    URI.Bookmark := '';

    CheckEquals('http://www.nevrona.com/path1/path2/path3/path4/', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/path1/path2/path3/path4/', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path1/path2/path3/path4/', URI.Path, 'Path');
    CheckEquals('', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest13;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2');
  try
    CheckEquals('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/cgi-bin/', URI.Path, 'Path');
    CheckEquals('example.cgi', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('?parameter=1&parameter=2', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest14;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/cgi-bin/';
    URI.Document := 'example.cgi';
    URI.Port     := '';
    URI.Username := '';
    URI.Password := '';
    URI.Params   := '?parameter=1&parameter=2';
    URI.Bookmark := '';

    CheckEquals('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2', URI.URI, 'URI');
    CheckEquals('http://www.nevrona.com/cgi-bin/example.cgi?parameter=1&parameter=2', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/cgi-bin/', URI.Path, 'Path');
    CheckEquals('example.cgi', URI.Document, 'Document');
    CheckEquals('', URI.Port, 'Port');
    CheckEquals('', URI.Username, 'Username');
    CheckEquals('', URI.Password, 'Password');
    CheckEquals('?parameter=1&parameter=2', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest15;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document#bookmark');
  try
    CheckEquals('http://www.nevrona.com:12345/path1/path2/path3/path4/document', URI.URI, 'URI');
    CheckEquals('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document#bookmark', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path1/path2/path3/path4/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('12345', URI.Port, 'Port');
    CheckEquals('username', URI.Username, 'Username');
    CheckEquals('password', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('bookmark', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest16;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document#bookmark');
  try
    URI.Protocol := 'http';
    URI.Host     := 'www.nevrona.com';
    URI.Path     := '/path1/path2/path3/path4/';
    URI.Document := 'document';
    URI.Port     := '12345';
    URI.Username := 'username';
    URI.Password := 'password';
    URI.Params   := '';
    URI.Bookmark := 'bookmark';

    CheckEquals('http://www.nevrona.com:12345/path1/path2/path3/path4/document', URI.URI, 'URI');
    CheckEquals('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document#bookmark', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path1/path2/path3/path4/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('12345', URI.Port, 'Port');
    CheckEquals('username', URI.Username, 'Username');
    CheckEquals('password', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('bookmark', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest17;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document?parameter=32&parameter=43#bookmark');
  try
    CheckEquals('http://www.nevrona.com:12345/path1/path2/path3/path4/document?parameter=32&parameter=43', URI.URI, 'URI');
    CheckEquals('http://username:password@www.nevrona.com:12345/path1/path2/path3/path4/document?parameter=32&parameter=43#bookmark', URI.GetFullURI, 'GetFullURI');
    CheckEquals('http', URI.Protocol, 'Protocol');
    CheckEquals('www.nevrona.com', URI.Host, 'Host');
    CheckEquals('/path1/path2/path3/path4/', URI.Path, 'Path');
    CheckEquals('document', URI.Document, 'Document');
    CheckEquals('12345', URI.Port, 'Port');
    CheckEquals('username', URI.Username, 'Username');
    CheckEquals('password', URI.Password, 'Password');
    CheckEquals('?parameter=32&parameter=43', URI.Params, 'Params');
    CheckEquals('bookmark', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

procedure TestTIdURI.TestTest18;
var
  URI: TIdURI;
begin
  URI := TIdURI.Create('ftp://aUser:aPassword@some.place.org:12345/aPath/aDocum');
  try
    CheckEquals('ftp://some.place.org:12345/aPath/aDocum', URI.URI, 'URI');
    CheckEquals('ftp://aUser:aPassword@some.place.org:12345/aPath/aDocum', URI.GetFullURI, 'GetFullURI');
    CheckEquals('ftp', URI.Protocol, 'Protocol');
    CheckEquals('some.place.org', URI.Host, 'Host');
    CheckEquals('/aPath/', URI.Path, 'Path');
    CheckEquals('aDocum', URI.Document, 'Document');
    CheckEquals('12345', URI.Port, 'Port');
    CheckEquals('aUser', URI.Username, 'Username');
    CheckEquals('aPassword', URI.Password, 'Password');
    CheckEquals('', URI.Params, 'Params');
    CheckEquals('', URI.Bookmark, 'Bookmark');
  finally
    URI.Free;
  end;
end;

initialization
  RegisterTest('Indy 9_0_14 TIdURI tests', Suite);
end.
