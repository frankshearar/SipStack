unit TestIdSipCore;

interface

uses
  IdSipCore, IdSipMessage, TestFramework;

type
  TestTIdSipUserAgentCore = class(TTestCase)
  private
    Core: TIdSipUserAgentCore;
  protected
    function CoreType: TIdSipUserAgentCoreClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
  end;

  TestTIdSipUserAgentClientCore = class(TestTIdSipUserAgentCore)
  protected
    function CoreType: TIdSipUserAgentCoreClass; override;
  published
    procedure TestCreateRequest;
    procedure TestCreateRequestSipsRequestUri;
  end;

  TestTIdSipUserAgentServerCore = class(TestTIdSipUserAgentCore)
  private
    P:       TIdSipParser;
    Request: TIdSipRequest;
  protected
    function CoreType: TIdSipUserAgentCoreClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateResponse;
    procedure TestCreateResponseRecordRoute;
    procedure TestCreateResponseSipsRecordRoute;
    procedure TestCreateResponseSipsRequestUri;
    procedure TestHandleUnmatchedRequestUnknownMethod;
  end;

implementation

uses
  IdSipConsts, IdSipHeaders, IdURI, TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipUserAgentCore.Suite);
  Result.AddTest(TestTIdSipUserAgentClientCore.Suite);
  Result.AddTest(TestTIdSipUserAgentServerCore.Suite);
end;

//******************************************************************************
//* TestTIdSipUserAgentCore                                                    *
//******************************************************************************
//* TestTIdSipUserAgentCore Public methods *************************************

procedure TestTIdSipUserAgentCore.SetUp;
var
  C: TIdSipContactHeader;
begin
  inherited SetUp;

  Self.Core := Self.CoreType.Create;

  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:wintermute@tessier-ashpool.co.lu';
    Self.Core.Contact := C;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TearDown;
begin
  Self.Core.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentCore Protected methods **********************************

function TestTIdSipUserAgentCore.CoreType: TIdSipUserAgentCoreClass;
begin
  Result := TIdSipUserAgentClientCore;
end;

//* TestTIdSipUserAgentCore Published methods **********************************

procedure TestTIdSipUserAgentCore.TestSetContact;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:case@fried.neurons.org';
    Self.Core.Contact := C;

    Check(Self.Core.Contact.IsEqualTo(C),
                'Contact not set');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContactMailTo;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := 'mailto:wintermute@tessier-ashpool.co.lu';
      Fail('Only a SIP or SIPs URI may be specified');
    except
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetContactWildCard;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    try
      C.Value := '*';
      Fail('Wildcard Contact headers make no sense in a response that sets up '
         + 'a dialog');
    except
    end;
  finally
    C.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipUserAgentClientCore                                              *
//******************************************************************************
//* TestTIdSipUserAgentClientCore Public methods *******************************

procedure TestTIdSipUserAgentClientCore.TestCreateRequest;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  URI:     TIdURI;
begin
  URI := TIdURI.Create('sip:wintermute@tessier-ashpool.co.lu');
  try
    Request := Self.Core.CreateRequest(URI);
    try
      CheckEquals(URI.GetFullURI,
                  Request.RequestUri,
                  'Request-URI not properly set');

      Check(Request.HasHeader(ContactHeaderFull), 'No Contact header added');

      Contact := Request.Headers[ContactHeaderFull] as TIdSipContactHeader;
      Check(Contact.IsEqualTo(Self.Core.Contact), 'Contact header incorrectly set');
    finally
      Request.Free;
    end;
  finally
    URI.Free;
  end;
end;

procedure TestTIdSipUserAgentClientCore.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  URI:     TIdURI;
begin
  URI := TIdURI.Create('sips:wintermute@tessier-ashpool.co.lu');
  try
    Request := Self.Core.CreateRequest(URI);
    try
      Contact := Request.Headers[ContactHeaderFull] as TIdSipContactHeader;
      CheckEquals(SipsScheme,
                  Contact.Address.Protocol,
                  'Contact doesn''t have a SIPS URI');
    finally
      Request.Free;
    end;
  finally
    URI.Free;
  end;
end;

//* TestTIdSipUserAgentClientCore Protected methods ****************************

function TestTIdSipUserAgentClientCore.CoreType: TIdSipUserAgentCoreClass;
begin
  Result := TIdSipUserAgentClientCore;
end;

//* TestTIdSipUserAgentClientCore Published methods ****************************

//******************************************************************************
//* TestTIdSipUserAgentServerCore                                              *
//******************************************************************************
//* TestTIdSipUserAgentServerCore Public methods *******************************

procedure TestTIdSipUserAgentServerCore.SetUp;
begin
  inherited SetUp;

  Self.P := TIdSipParser.Create;

  Self.Request := Self.P.ParseAndMakeRequest(BasicRequest);
end;

procedure TestTIdSipUserAgentServerCore.TearDown;
begin
  Self.Request.Free;
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentServerCore Protected methods ****************************

function TestTIdSipUserAgentServerCore.CoreType: TIdSipUserAgentCoreClass;
begin
  Result := TIdSipUserAgentServerCore;
end;

//* TestTIdSipUserAgentServerCore Published methods ****************************

procedure TestTIdSipUserAgentServerCore.TestCreateResponse;
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.Request);
  try
    Check(Response.CSeq.IsEqualTo(Request.CSeq),         'Cseq header mismatch');
    Check(Response.From.IsEqualTo(Request.From),         'From header mismatch');
    Check(Response.ToHeader.IsEqualTo(Request.ToHeader), 'To header mismatch');
    Check(Response.Path.IsEqualTo(Request.Path),         'Via headers mismatch');

    Check(Response.HasHeader(ContactHeaderFull), 'Missing Contact header');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentServerCore.TestCreateResponseRecordRoute;
var
  RequestRecordRoutes:  TIdSipHeadersFilter;
  Response:             TIdSipResponse;
  ResponseRecordRoutes: TIdSipHeadersFilter;
begin
  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6000>';
  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6001>';
  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sip:127.0.0.1:6002>';

  RequestRecordRoutes := TIdSipHeadersFilter.Create(Self.Request.Headers, RecordRouteHeader);
  try
    Response := Self.Core.CreateResponse(Self.Request);
    try
      ResponseRecordRoutes := TIdSipHeadersFilter.Create(Response.Headers, RecordRouteHeader);
      try
        Check(ResponseRecordRoutes.IsEqualTo(RequestRecordRoutes),
              'Record-Route header sets mismatch');
      finally
        ResponseRecordRoutes.Free;
      end;
    finally
      Response.Free;
    end;
  finally
    RequestRecordRoutes.Free;
  end;
end;

procedure TestTIdSipUserAgentServerCore.TestCreateResponseSipsRecordRoute;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sips:127.0.0.1:6000>';

  Response := Self.Core.CreateResponse(Self.Request);
  try
    Contact := Response.Headers[ContactHeaderFull] as TIdSipContactHeader;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentServerCore.TestCreateResponseSipsRequestUri;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
//  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sips:127.0.0.1:6000>';
  Self.Request.RequestUri := 'sips:wintermute@tessier-ashpool.co.lu';

  Response := Self.Core.CreateResponse(Self.Request);
  try
    Contact := Response.Headers[ContactHeaderFull] as TIdSipContactHeader;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentServerCore.TestHandleUnmatchedRequestUnknownMethod;
begin
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.