{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipOptionsModule;

interface

uses
  IdSipCore, IdSipMessage, IdSipOptionsModule, TestFrameworkSip,
  TestFrameworkSipTU;

type
  TestTIdSipOptionsModule = class(TTestCaseTU)
  private
    Module: TIdSipOptionsModule;

    procedure ReceiveOptions;
  public
    procedure SetUp; override;
  published
    procedure TestCreateOptions;
    procedure TestDoNotDisturb;
    procedure TestReceiveOptions;
    procedure TestRejectOptionsWithReplacesHeader;
  end;

  TestTIdSipOptionsAction = class(TestTIdSipAction)
  protected
    Module: TIdSipOptionsModule;
  public
    procedure SetUp; override;
  end;

  TestTIdSipInboundOptions = class(TestTIdSipOptionsAction)
  private
    Options:        TIdSipInboundOptions;
    OptionsRequest: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOutbound; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestOptions;
    procedure TestOptionsWhenDoNotDisturb;
    procedure TestTerminateSignalled; override;
  end;

  TestTIdSipOutboundOptions = class(TestTIdSipOptionsAction,
                                    IIdSipOptionsListener)
  private
    ReceivedResponse: Boolean;

    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddListener;
    procedure TestIsOptions; override;
    procedure TestReceiveResponse;
    procedure TestRemoveListener;
    procedure TestSendWithMaxForwards;
  end;

  TestTIdSipOptionsResponseMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipOptionsResponseMethod;
    Module: TIdSipOptionsModule;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  SysUtils, TestFramework, IdSipUserAgent;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipOptionsModule unit tests');
  Result.AddTest(TestTIdSipOptionsModule.Suite);
  Result.AddTest(TestTIdSipInboundOptions.Suite);
  Result.AddTest(TestTIdSipOutboundOptions.Suite);
  Result.AddTest(TestTIdSipOptionsResponseMethod.Suite);
end;

//******************************************************************************
//* TestTIdSipOptionsModule                                                    *
//******************************************************************************
//* TestTIdSipOptionsModule Public methods *************************************

procedure TestTIdSipOptionsModule.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor(MethodOptions) as TIdSipOptionsModule;
end;

//* TestTIdSipOptionsModule Private methods ************************************

procedure TestTIdSipOptionsModule.ReceiveOptions;
var
  Options: TIdSipRequest;
begin
  Options := Self.Module.CreateOptions(Self.Core.From, Self.Core.From, TIdSipRequest.DefaultMaxForwards);
  try
    Self.ReceiveRequest(Options);
  finally
    Options.Free;
  end;
end;

//* TestTIdSipOptionsModule Published methods **********************************

procedure TestTIdSipOptionsModule.TestCreateOptions;
const
  MaxForwards = 42;
var
  Options: TIdSipRequest;
begin
  Options := Self.Module.CreateOptions(Self.Core.From, Self.Destination, MaxForwards);
  try
    CheckEquals(MaxForwards,   Options.MaxForwards, 'Max-Forwards');
    CheckEquals(MethodOptions, Options.Method,      'Incorrect method');
    CheckEquals(MethodOptions, Options.CSeq.Method, 'Incorrect CSeq method');
    Check(Options.HasHeader(AcceptHeader),          'Missing Accept header');
    CheckEquals(Self.Core.AllowedContentTypes,
                Options.FirstHeader(AcceptHeader).Value,
                'Accept value');
  finally
    Options.Free;
  end;
end;

procedure TestTIdSipOptionsModule.TestDoNotDisturb;
begin
  Self.Core.DoNotDisturb := true;
  Self.MarkSentResponseCount;

  Self.ReceiveOptions;
  CheckResponseSent('No response sent when UA set to Do Not Disturb');

  CheckEquals(SIPTemporarilyUnavailable,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent: Do Not Disturb');
  CheckEquals(Self.Core.DoNotDisturbMessage,
              Self.LastSentResponse.StatusText,
              'Wrong status text: Do Not Disturb');
  Check(Self.LastSentResponse.HasWarning,
        'Response has no Warning, hence the request was rejected before the InboundOptions could process it');

  Self.Core.DoNotDisturb := false;

  Self.MarkSentResponseCount;
{
  Self.ReceiveOptions;
  CheckResponseSent('No response sent when UA not set to Do Not Disturb');

  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent: Do Not Disturb set off');
}
end;

procedure TestTIdSipOptionsModule.TestReceiveOptions;
var
  Options:  TIdSipRequest;
  Response: TIdSipResponse;
begin
  Options := TIdSipRequest.Create;
  try
    Options.Method := MethodOptions;
    Options.RequestUri.Uri := 'sip:franks@192.168.0.254';
    Options.AddHeader(ViaHeaderFull).Value  := 'SIP/2.0/UDP roke.angband.za.org:3442';
    Options.From.Value := '<sip:sipsak@roke.angband.za.org:3442>';
    Options.ToHeader.Value := '<sip:franks@192.168.0.254>';
    Options.CallID := '1631106896@roke.angband.za.org';
    Options.CSeq.Value := '1 OPTIONS';
    Options.AddHeader(ContactHeaderFull).Value := '<sip:sipsak@roke.angband.za.org:3442>';
    Options.ContentLength := 0;
    Options.MaxForwards := 0;
    Options.AddHeader(UserAgentHeader).Value := 'sipsak v0.8.1';

    Self.ReceiveRequest(Options);

    Response := Self.LastSentResponse;
    CheckEquals(SIPOK,
                Response.StatusCode,
                'We should accept all OPTIONS');
  finally
    Options.Free;
  end;
end;

procedure TestTIdSipOptionsModule.TestRejectOptionsWithReplacesHeader;
var
  Options: TIdSipRequest;
begin
  Options := Self.Module.CreateOptions(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Options.AddHeader(ReplacesHeader).Value := '1;from-tag=2;to-tag=3';

    Self.MarkSentResponseCount;
    Self.ReceiveRequest(Options);
    CheckResponseSent('No response sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                'Unexpected response');
  finally
    Options.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOptionsAction                                                    *
//******************************************************************************
//* TestTIdSipOptionsAction Public methods *************************************

procedure TestTIdSipOptionsAction.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor(MethodOptions) as TIdSipOptionsModule;
end;

//******************************************************************************
//* TestTIdSipInboundOptions                                                   *
//******************************************************************************
//* TestTIdSipInboundOptions Public methods ************************************

procedure TestTIdSipInboundOptions.SetUp;
begin
  inherited SetUp;

  Self.OptionsRequest := Self.Module.CreateOptions(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);

  Self.Options := TIdSipInboundOptions.CreateInbound(Self.Core,
                                                     Self.OptionsRequest,
                                                     Self.Binding);
end;

procedure TestTIdSipInboundOptions.TearDown;
begin
  Self.Options.Free;
  Self.OptionsRequest.Free;

  inherited TearDown;
end;

//* TestTIdSipInboundOptions Published methods *********************************

procedure TestTIdSipInboundOptions.TestIsInbound;
begin
  Check(Self.Options.IsInbound,
        Self.Options.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundOptions.TestIsInvite;
begin
  Check(not Self.Options.IsInvite,
          Self.Options.ClassName + ' marked as a Invite');
end;

procedure TestTIdSipInboundOptions.TestIsOptions;
begin
  Check(Self.Options.IsOptions,
        Self.Options.ClassName + ' not marked as an Options');
end;

procedure TestTIdSipInboundOptions.TestIsOutbound;
begin
  Check(not Self.Options.IsOutbound,
        Self.Options.ClassName + ' marked as outbound');
end;

procedure TestTIdSipInboundOptions.TestIsOwned;
begin
  Check(not Self.Options.IsOwned,
        Self.Options.ClassName + ' marked as being owned');
end;

procedure TestTIdSipInboundOptions.TestIsRegistration;
begin
  Check(not Self.Options.IsRegistration,
        Self.Options.ClassName + ' marked as a Registration');
end;

procedure TestTIdSipInboundOptions.TestIsSession;
begin
  Check(not Self.Options.IsSession,
        Self.Options.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundOptions.TestOptions;
var
  Response: TIdSipResponse;
begin
  Check(Self.SentResponseCount > 0,
        'No response sent');

  Response := Self.LastSentResponse;
  Check(Response.HasHeader(AllowHeader),
        'No Allow header');
  CheckEquals(Self.Core.KnownMethods,
              Response.FirstHeader(AllowHeader).FullValue,
              'Allow header');

  Check(Response.HasHeader(AcceptHeader),
        'No Accept header');
  CheckEquals(Self.Core.AllowedContentTypes,
              Response.FirstHeader(AcceptHeader).FullValue,
              'Accept header');

  Check(Response.HasHeader(AcceptEncodingHeader),
        'No Accept-Encoding header');
  CheckEquals(Self.Core.AllowedEncodings,
              Response.FirstHeader(AcceptEncodingHeader).FullValue,
              'Accept-Encoding header');

  Check(Response.HasHeader(AcceptLanguageHeader),
        'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).FullValue,
              'Accept-Language header');

  Check(Response.HasHeader(SupportedHeaderFull),
        'No Supported header');
  CheckEquals(Self.Core.AllowedExtensions,
              Response.FirstHeader(SupportedHeaderFull).FullValue,
              'Supported header value');

  Check(Response.HasHeader(ContactHeaderFull),
        'No Contact header');
  CheckEquals(Self.Options.LocalGruu.AsString,
              Response.FirstContact.AsString,
        'Contact header value');

  Check(Response.HasHeader(WarningHeader),
        'No Warning header');
  CheckEquals(Self.Binding.LocalIP,
              Response.FirstWarning.Agent,
              'Warning warn-agent');
end;

procedure TestTIdSipInboundOptions.TestOptionsWhenDoNotDisturb;
var
  NewOptions: TIdSipInboundOptions;
  Response:   TIdSipResponse;
begin
  Self.Core.DoNotDisturb := true;

  Self.MarkSentResponseCount;
  NewOptions := TIdSipInboundOptions.CreateInbound(Self.Core,
                                                   Self.Options.InitialRequest,
                                                   Self.Binding);
  try
    CheckResponseSent('No response sent');

    Response := Self.LastSentResponse;
    CheckEquals(SIPTemporarilyUnavailable,
                Response.StatusCode,
                'Do Not Disturb');
  finally
    NewOptions.Free;
  end;
end;

procedure TestTIdSipInboundOptions.TestTerminateSignalled;
begin
  Check(true, 'TIdSipInboundOptions terminates as soon as it receives a message: you can''t attach a listener to it. Thus this test makes no sense.');
end;

//******************************************************************************
//* TestTIdSipOutboundOptions                                                  *
//******************************************************************************
//* TestTIdSipOutboundOptions Public methods ***********************************

procedure TestTIdSipOutboundOptions.SetUp;
begin
  inherited SetUp;

  Self.ReceivedResponse := false;
end;

//* TestTIdSipOutboundOptions Protected methods ********************************

function TestTIdSipOutboundOptions.CreateAction: TIdSipAction;
var
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Module.QueryOptions(Self.Destination);
  Options.AddActionListener(Self);
  Options.AddListener(Self);
  Options.Send;
  Result := Options;
end;

//* TestTIdSipOutboundOptions Private methods **********************************

procedure TestTIdSipOutboundOptions.OnResponse(OptionsAgent: TIdSipOutboundOptions;
                                               Response: TIdSipResponse);
begin
  Self.ReceivedResponse := true;
end;

//* TestTIdSipOutboundOptions Published methods ********************************

procedure TestTIdSipOutboundOptions.TestAddListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Module.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response, 'L1 not informed of response');
      Check(L2.Response, 'L2 not informed of response');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundOptions.TestIsOptions;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(Action.IsOptions,
        Action.ClassName + ' marked as an Options');
end;

procedure TestTIdSipOutboundOptions.TestReceiveResponse;
var
  OptionsCount: Integer;
  StatusCode:   Cardinal;
begin
  for StatusCode := SIPOKResponseClass to SIPGlobalFailureResponseClass do begin
    Self.ReceivedResponse := false;
    Self.CreateAction;

    OptionsCount := Self.Core.CountOf(MethodOptions);

    Self.ReceiveResponse(StatusCode * 100);

    Check(Self.ReceivedResponse,
          'Listeners not notified of response ' + IntToStr(StatusCode * 100));
    Check(Self.Core.CountOf(MethodOptions) < OptionsCount,
          'OPTIONS action not terminated for ' + IntToStr(StatusCode) + ' response');
  end;
end;

procedure TestTIdSipOutboundOptions.TestRemoveListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Module.QueryOptions(Self.Core.From);
  Options.Send;

  L1 := TIdSipTestOptionsListener.Create;
  try
    L2 := TIdSipTestOptionsListener.Create;
    try
      Options.AddListener(L1);
      Options.AddListener(L2);
      Options.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Response,
            'First listener not notified');
      Check(not L2.Response,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipOutboundOptions.TestSendWithMaxForwards;
const
  MaxForwards = 42;
var
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Module.QueryOptions(Self.Core.From);
  Options.MaxForwards := MaxForwards;

  Self.MarkSentRequestCount;
  Options.Send;
  CheckRequestSent('No OPTIONS sent');
  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

//******************************************************************************
//* TestTIdSipOptionsResponseMethod                                            *
//******************************************************************************
//* TestTIdSipOptionsResponseMethod Public methods *****************************

procedure TestTIdSipOptionsResponseMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Self.Method := TIdSipOptionsResponseMethod.Create;

  Self.Module := Self.UA.ModuleFor(MethodOptions) as TIdSipOptionsModule;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Options  := Self.Module.QueryOptions(Nowhere);
    Self.Method.Response := Self.Response;
  finally
    Nowhere.Free;
  end;
end;

procedure TestTIdSipOptionsResponseMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOptionsResponseMethod Published methods **************************

procedure TestTIdSipOptionsResponseMethod.TestRun;
var
  Listener: TIdSipTestOptionsListener;
begin
  Listener := TIdSipTestOptionsListener.Create;
  try
    Self.Method.Run(Listener);

    Check(Listener.Response, 'Listener not notified');
    Check(Self.Method.Options = Listener.OptionsAgentParam,
          'OptionsAgent param');
    Check(Self.Method.Response = Listener.ResponseParam,
          'Response param');
  finally
    Listener.Free;
  end;
end;

initialization
  RegisterTest('Options Module tests', Suite);
end.
