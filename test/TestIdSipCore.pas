unit TestIdSipCore;

interface

uses
  IdSipCore, IdSipHeaders, IdSipMessage, IdSipTransaction, IdURI, TestFramework;

type
  TestTIdSipAbstractCore = class(TTestCase)
  private
    Core: TIdSipAbstractCore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNextCallIDGStackInstantiated;
    procedure TestNextCallIDNoGStack;
  end;

  TestTIdSipUserAgentCore = class(TTestCase)
  private
    Core:     TIdSipUserAgentCore;
    Dispatch: TIdSipMockTransactionDispatcher;
    P:        TIdSipParser;
    Request:  TIdSipRequest;

    procedure CheckCreateRequest(const Dest: TIdSipToHeader; const Request: TIdSipRequest);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCall;
    procedure TestCreateInvite;
    procedure TestCreateRequest;
    procedure TestCreateRequestSipsRequestUri;
    procedure TestCreateResponse;
    procedure TestCreateResponseRecordRoute;
    procedure TestCreateResponseSipsRecordRoute;
    procedure TestCreateResponseSipsRequestUri;
//    procedure TestHandleUnmatchedRequestUnknownMethod;
    procedure TestNextTag;
    procedure TestSetContact;
    procedure TestSetContactMailto;
    procedure TestSetContactWildCard;
    procedure TestSetFrom;
    procedure TestSetFromMailto;
  end;

implementation

uses
  Classes, IdException, IdGlobal, IdHttp, IdSipConsts, IdStack, SysUtils,
  TestMessages;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipUserAgentCore.Suite);
end;

//******************************************************************************
//* TestTIdSipAbstractCore                                                     *
//******************************************************************************
//* TestTIdSipAbstractCore Public methods **************************************

// Self.Core is an (almost) abstract base class. We want to test the (static)
// methods that are not abstract, and we want to do this without the compiler
// moaning about something we know to be safe.
{$WARNINGS OFF}
procedure TestTIdSipAbstractCore.SetUp;
begin
  inherited SetUp;

  Self.Core := TIdSipAbstractCore.Create;
end;
{$WARNINGS ON}

procedure TestTIdSipAbstractCore.TearDown;
begin
  Self.Core.Free;

  inherited TearDown;
end;

//* TestTIdSipAbstractCore Published methods ***********************************

procedure TestTIdSipAbstractCore.TestNextCallIDGStackInstantiated;
var
  CallID: String;
  H:      TIdHttp;
begin
  H := TIdHttp.Create(nil);
  try
    try
      H.Get('http://localhost/');
    except
      on EIdException do;
    end;

    CallID := Self.Core.NextCallID;

    Fetch(CallID, '@');

    CheckEquals(GStack.LocalAddress, CallID, 'Local machine name not used');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestNextCallIDNoGStack;
var
  CallID: String;
begin
  CallID := Self.Core.NextCallID;

  CheckNotEquals('',     CallID,               'Null Call-ID');
  CheckNotEquals(CallID, Self.Core.NextCallID, 'Repeated Call-ID');

  Fetch(CallID, '@');
  CheckEquals('localhost',
              CallID,
              '''localhost'' not appended to Call-ID when GStack not '
            + 'instantiated');
end;

//******************************************************************************
//* TestTIdSipUserAgentCore                                                    *
//******************************************************************************
//* TestTIdSipUserAgentCore Public methods *************************************

procedure TestTIdSipUserAgentCore.SetUp;
var
  C: TIdSipContactHeader;
  F: TIdSipFromHeader;
begin
  inherited SetUp;

  Self.Dispatch := TIdSipMockTransactionDispatcher.Create;
  Self.Core := TIdSipUserAgentCore.Create;
  Self.Core.Dispatcher := Self.Dispatch;

  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:wintermute@tessier-ashpool.co.lu';
    Self.Core.Contact := C;
  finally
    C.Free;
  end;

  F := TIdSipFromHeader.Create;
  try
    F.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.lu>';
    Self.Core.From := F;
  finally
    F.Free;
  end;

  Self.P := TIdSipParser.Create;

  Self.Request := Self.P.ParseAndMakeRequest(LocalLoopRequest);
end;

procedure TestTIdSipUserAgentCore.TearDown;
begin
  Self.Request.Free;
  Self.P.Free;
  Self.Core.Free;
  Self.Dispatch.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentCore Private methods ************************************

procedure TestTIdSipUserAgentCore.CheckCreateRequest(const Dest: TIdSipToHeader; const Request: TIdSipRequest);
var
  Contact: TIdSipContactHeader;
begin
  CheckEquals(Dest.Address.GetFullURI,
              Request.RequestUri,
              'Request-URI not properly set');

  Check(Request.HasHeader(CallIDHeaderFull), 'No Call-ID header added');
  CheckNotEquals('',
                 (Request.Headers[CallIDHeaderFull] as TIdSipCallIdHeader).Value,
                 'Call-ID must not be empty');

  Check(Request.HasHeader(ContactHeaderFull), 'No Contact header added');
  Contact := Request.Headers[ContactHeaderFull] as TIdSipContactHeader;
  Check(Contact.IsEqualTo(Self.Core.Contact), 'Contact header incorrectly set');

  CheckEquals(Request.From.DisplayName,
              Self.Core.From.DisplayName,
              'From.DisplayName');
  CheckEquals(Request.From.Address.GetFullURI,
              Self.Core.From.Address.GetFullURI,
              'From.Address.GetFullURI');
    CheckNotEquals('',
                   Request.From.Tag,
                   'Requests MUST have a From tag; cf. RFC 3261 section 8.1.1.3');

  CheckEquals(Request.RequestUri,
              Request.ToHeader.Address.GetFullURI,
              'To header incorrectly set');

  CheckEquals(1,
              Request.Path.Length,
              'New requests MUST have a Via header; cf. RFC 3261 section 8.1.1.7');
  Check(Request.LastHop.HasBranch,
        'New requests MUST have a branch; cf. RFC 3261 section 8.1.1.7');

end;

//* TestTIdSipUserAgentCore Published methods **********************************

procedure TestTIdSipUserAgentCore.TestCall;
var
  Destination:          TIdSipToHeader;
  OriginalRequestCount: Integer;
begin
  OriginalRequestCount := Self.Dispatch.Transport.SentRequestCount;

  Destination := TIdSipToHeader.Create;
  try
    Destination.Value := 'sip:franks@localhost';
    Self.Core.Call(Destination);

    CheckEquals(OriginalRequestCount + 1,
                Self.Dispatch.Transport.SentRequestCount,
                'no INVITE sent');
  finally
    Destination.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateInvite;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateInvite(Dest);
    try
      Self.CheckCreateRequest(Dest, Request);
      CheckEquals(MethodInvite, Request.Method, 'Incorrect method');

      CheckEquals('',
                  Request.ToHeader.Tag,
                  'This request is outside of a dialog, hence MUST NOT have a '
                + 'To tag. See RFC:3261, section 8.1.1.2');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequest;
var
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sip:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateRequest(Dest);
    try
      Self.CheckCreateRequest(Dest, Request);
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateRequestSipsRequestUri;
var
  Contact: TIdSipContactHeader;
  Request: TIdSipRequest;
  Dest:    TIdSipToHeader;
begin
  Dest := TIdSipToHeader.Create;
  try
    Dest.Address.URI := 'sips:wintermute@tessier-ashpool.co.lu';
    Request := Self.Core.CreateRequest(Dest);
    try
      Contact := Request.Headers[ContactHeaderFull] as TIdSipContactHeader;
      CheckEquals(SipsScheme,
                  Contact.Address.Protocol,
                  'Contact doesn''t have a SIPS URI');
    finally
      Request.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponse;
var
  Response: TIdSipResponse;
begin
  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    CheckEquals(SIPOK, Response.StatusCode,              'StatusCode mismatch');
    Check(Response.CSeq.IsEqualTo(Request.CSeq),         'Cseq header mismatch');
    Check(Response.From.IsEqualTo(Request.From),         'From header mismatch');
    Check(Response.ToHeader.IsEqualTo(Request.ToHeader), 'To header mismatch');
    Check(Response.Path.IsEqualTo(Request.Path),         'Via headers mismatch');

    Check(Response.HasHeader(ContactHeaderFull), 'Missing Contact header');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseRecordRoute;
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
    Response := Self.Core.CreateResponse(Self.Request, SIPOK);
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

procedure TestTIdSipUserAgentCore.TestCreateResponseSipsRecordRoute;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
  Self.Request.Headers.Add(RecordRouteHeader).Value := '<sips:127.0.0.1:6000>';

  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    Contact := Response.Headers[ContactHeaderFull] as TIdSipContactHeader;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestCreateResponseSipsRequestUri;
var
  Contact:  TIdSipContactHeader;
  Response: TIdSipResponse;
begin
  Self.Request.RequestUri := 'sips:wintermute@tessier-ashpool.co.lu';

  Response := Self.Core.CreateResponse(Self.Request, SIPOK);
  try
    Contact := Response.Headers[ContactHeaderFull] as TIdSipContactHeader;
    CheckEquals(SipsScheme, Contact.Address.Protocol,
                'Must use a SIPS URI in the Contact');
  finally
    Response.Free;
  end;
end;
{
procedure TestTIdSipUserAgentCore.TestHandleUnmatchedRequestUnknownMethod;
begin
  Fail('not implemented yet');
end;
}
procedure TestTIdSipUserAgentCore.TestNextTag;
var
  I:    Integer;
  Tags: TStringList;
begin
  // This is a woefully inadequate test. cf. RFC 3261, section 19.3

  Tags := TStringList.Create;
  try
    for I := 1 to 100 do
      Tags.Add(Self.Core.NextTag);

    // Find duplicates
    Tags.Sort;
    CheckNotEquals('', Tags[0], 'No null tags may be generated');

    for I := 1 to Tags.Count - 1 do begin
      CheckNotEquals('', Tags[I], 'No null tags may be generated (Tag #'
                                + IntToStr(I) + ')');

      CheckNotEquals(Tags[I-1], Tags[I], 'Duplicate tag generated');
    end;
  finally
  end;
end;

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
      Self.Core.Contact := C;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EAssertionFailed do;
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

procedure TestTIdSipUserAgentCore.TestSetFrom;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    F.Value := 'sip:case@fried.neurons.org';
    Self.Core.From := F;

    Check(Self.Core.From.IsEqualTo(F),
                'From not set');
  finally
    F.Free;
  end;
end;

procedure TestTIdSipUserAgentCore.TestSetFromMailTo;
var
  F: TIdSipFromHeader;
begin
  F := TIdSipFromHeader.Create;
  try
    try
      F.Value := 'mailto:wintermute@tessier-ashpool.co.lu';
      Self.Core.From := F;
      Fail('Only a SIP or SIPs URI may be specified');
    except
      on EAssertionFailed do;
    end;
  finally
    F.Free;
  end;
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.
