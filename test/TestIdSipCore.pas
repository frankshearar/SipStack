{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipCore;

interface

uses
  IdObservable, IdSipCore, IdSipDialog, IdSipInviteModule, IdSipMessage,
  IdSipMockTransactionDispatcher, IdSipTransport, IdSipUserAgent,
  TestFramework, TestFrameworkSip, TestFrameworkSipTU;

type
  TestTIdSipAbstractCore = class(TTestCaseTU,
                                 IIdSipTransactionUserListener)
  private
    ScheduledEventFired: Boolean;

    procedure CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                         Header: TIdSipHeader;
                                         const Msg: String);
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                        Challenge: TIdSipResponse;
                                        var Username: String;
                                        var Password: String;
                                        var TryAgain: Boolean); overload;
    procedure OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                        ChallengedRequest: TIdSipRequest;
                                        Challenge: TIdSipResponse); overload;
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Receiver: TIdSipTransport);
    procedure ScheduledEvent(Sender: TObject);
  public
    procedure SetUp; override;
  published
    procedure TestAddAllowedLanguage;
    procedure TestAddAllowedLanguageLanguageAlreadyPresent;
    procedure TestAddAllowedMethod;
    procedure TestAddAllowedMethodMethodAlreadyPresent;
    procedure TestAddAllowedScheme;
    procedure TestAddAllowedSchemeSchemeAlreadyPresent;
    procedure TestAddModule;
    procedure TestAddObserver;
    procedure TestAllowedExtensionsCollectsExtensionsFromInstalledModules;
    procedure TestAllowedExtensionsRemovesDuplicateExtensions;
    procedure TestCreateRequestFromUriWithDangerousHeaders;
    procedure TestCreateRequestFromUriWithDifferentMethod;
    procedure TestCreateRequestFromUriWithFalseAdvertising;
    procedure TestCreateRequestFromUriWithHeaders;
    procedure TestCreateRequestFromUriWithKnownParams;
    procedure TestCreateRequestFromUriWithMalformedHeader;
    procedure TestCreateRequestFromUriWithMethod;
    procedure TestCreateRequestFromUriWithUnknownParam;
    procedure TestCreateRequestWithGruu;
    procedure TestHasUnknownContentEncoding;
    procedure TestHasUnsupportedExtension;
    procedure TestIsMethodSupported;
    procedure TestIsSchemeAllowed;
    procedure TestLoopDetection;
    procedure TestModuleForString;
    procedure TestModuleForClassType;
    procedure TestNextCallID;
    procedure TestNextTag;
    procedure TestNotifyOfChange;
    procedure TestRejectUnknownContentEncoding;
    procedure TestRejectUnknownContentLanguage;
    procedure TestRejectUnknownExtension;
    procedure TestRejectUnknownScheme;
    procedure TestRejectUnsupportedMethod;
    procedure TestRejectUnsupportedSipVersion;
    procedure TestRemoveObserver;
    procedure TestRequiresUnsupportedExtension;
    procedure TestScheduleEvent;
    procedure TestSendRequest;
    procedure TestSendRequestMalformedRequest;
    procedure TestSendRequestUnknownMethod;
    procedure TestSendRequestUnknownRequiredExtension;
    procedure TestSendRequestUnknownSupportedExtension;
    procedure TestSendResponse;
    procedure TestSendResponseMalformedResponse;
    procedure TestSendResponseUnknownSupportedExtension;
    procedure TestSetInstanceID;
    procedure TestUseGruuAffectsAllowedExtensions;
  end;

  TIdSipNullAction = class(TIdSipAction)
  protected
    function CreateNewAttempt: TIdSipRequest; override;
  public
    function Method: String; override;
  end;

  TestTIdSipActions = class(TTestCaseTU)
  private
    ActionProcUsed:      String;
    Actions:             TIdSipActions;
    DidntFindActionName: String;
    FoundActionName:     String;
    Options:             TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestActionCount;
    procedure TestAddActionNotifiesObservers;
    procedure TestAddObserver;
    procedure TestCleanOutTerminatedActions;
    procedure TestFindActionForGruu;
    procedure TestFindActionForGruuNoActions;
    procedure TestFindActionForGruuWithOwnedActions;
    procedure TestFindActionAndPerformBlock;
    procedure TestFindActionAndPerformBlockNoActions;
    procedure TestFindActionAndPerformBlockNoMatch;
    procedure TestFindActionAndPerformOrBlock;
    procedure TestFindActionAndPerformOrBlockNoMatch;
    procedure TestInviteCount;
    procedure TestRemoveObserver;
    procedure TestTerminateAllActions;
  end;

  // These tests exercise the SIP discovery algorithms as defined in RFC 3263.
  TestLocation = class(TTestCaseTU,
                       IIdSipActionListener,
                       IIdSipOwnedActionListener,
                       IIdSipInviteListener)
  private
    InviteOffer:    String;
    InviteMimeType: String;
    NetworkFailure: Boolean;
    TransportParam: String;

    function  CreateAction: TIdSipOutboundInitialInvite;
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                        Response: TIdSipResponse);
    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                  NewDialog: TIdSipDialog);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
  public
    procedure SetUp; override;
  published
    procedure TestAllLocationsFail;
    procedure TestLooseRoutingProxy;
    procedure TestStrictRoutingProxy;
    procedure TestUseCorrectTransport;
    procedure TestUseTransportParam;
    procedure TestUseUdpByDefault;
    procedure TestVeryLargeMessagesUseAReliableTransport;
  end;

  TestTIdSipMessageModule = class(TTestCaseTU)
  private
    Module: TIdSipMessageModule;
  public
    procedure SetUp; override;
  published
    procedure TestAddAllowedContentType;
    procedure TestAddAllowedContentTypeMalformed;
    procedure TestAddAllowedContentTypes;
    procedure TestHasKnownAcceptDoesntAffectRequestParam;
    procedure TestHasKnownAcceptEmptyAcceptHeader;
    procedure TestHasKnownAcceptNoAcceptableMimeTypes;
    procedure TestHasKnownAcceptNoAcceptHeader;
    procedure TestHasKnownAcceptWithKnownMimeType;
    procedure TestHasKnownAcceptWithKnownMimeTypeAmongOthers;
    procedure TestHasUnknownContentType;
    procedure TestRejectNonInviteWithReplacesHeader;
    procedure TestRejectNoSupportedMimeTypesInAccept;
  end;

  TestTIdSipNullModule = class(TTestCaseTU)
  private
    Module: TIdSipMessageModule;
  public
    procedure SetUp; override;
  published
    procedure TestIsNull;
  end;

  TestTIdSipRedirectedAction = class(TTestCaseTU)
  private
    Action: TIdSipRedirectedAction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMethodSetMethod;
  end;

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

  TestTIdSipInboundOptions = class(TestTIdSipAction)
  private
    Options: TIdSipInboundOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestOptions;
    procedure TestOptionsWhenDoNotDisturb;
  end;

  TestTIdSipOutboundOptions = class(TestTIdSipAction,
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
  end;

  TestTIdSipActionRegistry = class(TTestCase)
  private
    Core:    TIdSipUserAgent;
    Request: TIdSipRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestActionsAddToRegistryAutomatically;
    procedure TestActionsGetUniqueIDs;
    procedure TestActionsAutomaticallyUnregister;
  end;

  TIdSipActionWaitTestCase = class(TTestCaseTU)
  end;

  TestTIdSipActionSendWait = class(TTestCaseTU)
  private
    Action: TIdSipAction;
    Wait:   TIdSipActionSendWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
  end;

  TestTIdSipActionTerminateWait = class(TTestCaseTU)
  private
    Action: TIdSipAction;
    Wait:   TIdSipActionTerminateWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerOnNonExistentAction;
  end;

  TestTIdSipActionAuthenticationChallengeMethod = class(TActionMethodTestCase)
  private
    Action:   TIdSipAction;
    Listener: TIdSipMockListener;
    Method:   TIdSipActionAuthenticationChallengeMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipActionNetworkFailureMethod = class(TActionMethodTestCase)
  private
    Action:    TIdSipAction;
    ErrorCode: Cardinal;
    Listener:  TIdSipMockListener;
    Method:    TIdSipActionNetworkFailureMethod;
    Reason:    String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TOwnedActionMethodTestCase = class(TActionMethodTestCase)
  protected
    Action:   TIdSipAction;
    Listener: TIdSipOwnedActionListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipOwnedActionFailureMethod = class(TOwnedActionMethodTestCase)
  private
    Method: TIdSipOwnedActionFailureMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOwnedActionRedirectMethod = class(TOwnedActionMethodTestCase)
  private
    Method: TIdSipOwnedActionRedirectMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Run;
  end;

  TestTIdSipOwnedActionSuccessMethod = class(TOwnedActionMethodTestCase)
  private
    Method: TIdSipOwnedActionSuccessMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipOptionsResponseMethod = class(TActionMethodTestCase)
  private
    Method: TIdSipOptionsResponseMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipActionRedirectorMethod = class(TActionMethodTestCase)
  private
    Listener:   TIdSipMockActionRedirectorListener;
    Redirector: TIdSipActionRedirector;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipRedirectorRedirectFailureMethod = class(TestTIdSipActionRedirectorMethod)
  private
    ErrorCode:  Cardinal;
    Method:     TIdSipRedirectorRedirectFailureMethod;
    Reason:     String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipRedirectorNewActionMethod = class(TestTIdSipActionRedirectorMethod)
  private
    Method:    TIdSipRedirectorNewActionMethod;
    NewAction: TIdSipAction;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipRedirectorSuccessMethod = class(TestTIdSipActionRedirectorMethod)
  private
    Method:   TIdSipRedirectorSuccessMethod;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipUserAgentDroppedUnmatchedMessageMethod = class(TTestCase)
  private
    Method:   TIdSipUserAgentDroppedUnmatchedMessageMethod;
    Receiver: TIdSipTransport;
    Response: TIdSipResponse;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

implementation

uses
  Classes, IdException, IdSdp, IdSimpleParser, IdSipDns, IdSipLocator,
  IdSipMockTransport, IdSipRegistration, IdSipSubscribeModule, SysUtils;

const
  DefaultTimeout = 5000;

type
  TIdSipCoreWithExposedNotify = class(TIdSipAbstractCore)
  public
    procedure TriggerNotify;
  end;

  TIdSipDoubleExtensionMessageModule = class(TIdSipMessageModule)
  public
    function AllowedExtensions: String; override;
  end;

  TIdSipMultipleExtensionMessageModule = class(TIdSipMessageModule)
  public
    function AllowedExtensions: String; override;
  end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipCore unit tests');
  Result.AddTest(TestTIdSipAbstractCore.Suite);
  Result.AddTest(TestTIdSipActions.Suite);
  Result.AddTest(TestLocation.Suite);
  Result.AddTest(TestTIdSipMessageModule.Suite);
  Result.AddTest(TestTIdSipNullModule.Suite);
  Result.AddTest(TestTIdSipRedirectedAction.Suite);
  Result.AddTest(TestTIdSipOptionsModule.Suite);
  Result.AddTest(TestTIdSipInboundOptions.Suite);
  Result.AddTest(TestTIdSipOutboundOptions.Suite);
  Result.AddTest(TestTIdSipActionRegistry.Suite);
  Result.AddTest(TestTIdSipActionSendWait.Suite);
  Result.AddTest(TestTIdSipActionTerminateWait.Suite);
  Result.AddTest(TestTIdSipActionAuthenticationChallengeMethod.Suite);
  Result.AddTest(TestTIdSipActionNetworkFailureMethod.Suite);
  Result.AddTest(TestTIdSipOwnedActionFailureMethod.Suite);
  Result.AddTest(TestTIdSipOwnedActionRedirectMethod.Suite);
  Result.AddTest(TestTIdSipOwnedActionSuccessMethod.Suite);
  Result.AddTest(TestTIdSipOptionsResponseMethod.Suite);
  Result.AddTest(TestTIdSipRedirectorRedirectFailureMethod.Suite);
  Result.AddTest(TestTIdSipRedirectorNewActionMethod.Suite);
  Result.AddTest(TestTIdSipRedirectorSuccessMethod.Suite);
  Result.AddTest(TestTIdSipUserAgentDroppedUnmatchedMessageMethod.Suite);
end;

//******************************************************************************
//* TIdSipCoreWithExposedNotify                                                *
//******************************************************************************
//* TIdSipCoreWithExposedNotify Public methods *********************************

procedure TIdSipCoreWithExposedNotify.TriggerNotify;
begin
  Self.NotifyOfChange;
end;

//******************************************************************************
//* TIdSipDoubleExtensionMessageModule                                         *
//******************************************************************************
//* TIdSipDoubleExtensionMessageModule Public methods **************************

function TIdSipDoubleExtensionMessageModule.AllowedExtensions: String;
begin
  Result := 'foo, foo';
end;

//******************************************************************************
//* TIdSipMultipleExtensionMessageModule                                       *
//******************************************************************************
//* TIdSipMultipleExtensionMessageModule Public methods ************************

function TIdSipMultipleExtensionMessageModule.AllowedExtensions: String;
begin
  Result := 'foo, bar, baz';
end;

//******************************************************************************
//* TestTIdSipAbstractCore                                                     *
//******************************************************************************
//* TestTIdSipAbstractCore Public methods **************************************

procedure TestTIdSipAbstractCore.SetUp;
begin
  inherited SetUp;

  Self.ScheduledEventFired := false;
end;

//* TestTIdSipAbstractCore Private methods *************************************

procedure TestTIdSipAbstractCore.CheckCommaSeparatedHeaders(const ExpectedValues: String;
                                                            Header: TIdSipHeader;
                                                            const Msg: String);
var
  Hdr:    TIdSipCommaSeparatedHeader;
  I:      Integer;
  Values: TStringList;
begin
  CheckEquals(TIdSipCommaSeparatedHeader.ClassName,
              Header.ClassName,
              Msg + ': Unexpected header type in CheckCommaSeparatedHeaders');

  Hdr := Header as TIdSipCommaSeparatedHeader;
  Values := TStringList.Create;
  try
    Values.CommaText := ExpectedValues;

    for I := 0 to Values.Count - 1 do
      CheckEquals(Values[I],
                  Hdr.Values[I],
                  Msg + ': ' + IntToStr(I + 1) + 'th value');
  finally
    Values.Free;
  end;
end;

procedure TestTIdSipAbstractCore.OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                                           Challenge: TIdSipResponse;
                                                           var Username: String;
                                                           var Password: String;
                                                           var TryAgain: Boolean);
begin
end;

procedure TestTIdSipAbstractCore.OnAuthenticationChallenge(UserAgent: TIdSipAbstractCore;
                                                           ChallengedRequest: TIdSipRequest;
                                                           Challenge: TIdSipResponse);
begin
end;


procedure TestTIdSipAbstractCore.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                           Message: TIdSipMessage;
                                                           Receiver: TIdSipTransport);
begin
end;

procedure TestTIdSipAbstractCore.ScheduledEvent(Sender: TObject);
begin
  Self.ScheduledEventFired := true;
  Self.ThreadEvent.SetEvent;
end;


//* TestTIdSipAbstractCore Published methods ***********************************

procedure TestTIdSipAbstractCore.TestAddAllowedLanguage;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('af');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(2, Languages.Count, 'Number of allowed Languages');

    CheckEquals('en', Languages[0], 'en first');
    CheckEquals('af', Languages[1], 'af second');
  finally
    Languages.Free;
  end;

  try
    Self.Core.AddAllowedLanguage(' ');
    Fail('Failed to forbid adding a malformed language ID');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddAllowedLanguageLanguageAlreadyPresent;
var
  Languages: TStrings;
begin
  Languages := TStringList.Create;
  try
    Self.Core.AddAllowedLanguage('en');
    Self.Core.AddAllowedLanguage('en');

    Languages.CommaText := Self.Core.AllowedLanguages;

    CheckEquals(1, Languages.Count, 'en was re-added');
  finally
    Languages.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddAllowedMethod;
var
  Methods: TStringList;
begin
  Methods := TStringList.Create;
  try
    Methods.CommaText := Self.Core.KnownMethods;
    Methods.Sort;

    CheckEquals(MethodAck,     Methods[0], 'ACK first');
    CheckEquals(MethodBye,     Methods[1], 'BYE second');
    CheckEquals(MethodCancel,  Methods[2], 'CANCEL third');
    CheckEquals(MethodInvite,  Methods[3], 'INVITE fourth');
    CheckEquals(MethodOptions, Methods[4], 'OPTIONS fifth');

    CheckEquals(5, Methods.Count, 'Number of allowed methods');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddAllowedMethodMethodAlreadyPresent;
var
  Methods: TStrings;
  MethodCount: Cardinal;
begin
  Methods := TStringList.Create;
  try
    Self.Core.AddModule(TIdSipInviteModule);
    Methods.CommaText := Self.Core.KnownMethods;
    MethodCount := Methods.Count;

    Self.Core.AddModule(TIdSipInviteModule);
    Methods.CommaText := Self.Core.KnownMethods;

    CheckEquals(MethodCount, Methods.Count, MethodInvite + ' was re-added');
  finally
    Methods.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddAllowedScheme;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipScheme);
    Self.Core.AddAllowedScheme(SipsScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(2, Schemes.Count, 'Number of allowed Schemes');

    CheckEquals(SipScheme,  Schemes[0], 'SIP first');
    CheckEquals(SipsScheme, Schemes[1], 'SIPS second');
  finally
    Schemes.Free;
  end;

  try
    Self.Core.AddAllowedScheme(' ');
    Fail('Failed to forbid adding a malformed URI scheme');
  except
    on EIdException do;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddAllowedSchemeSchemeAlreadyPresent;
var
  Schemes: TStrings;
begin
  Schemes := TStringList.Create;
  try
    Self.Core.AddAllowedScheme(SipScheme);

    Schemes.CommaText := Self.Core.AllowedSchemes;

    CheckEquals(1, Schemes.Count, 'SipScheme was re-added');
  finally
    Schemes.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAddModule;
var
  Module:     TIdSipMessageModule;
  ModuleType: TIdSipMessageModuleClass;
begin
  ModuleType := TIdSipSubscribeModule;

  Module := Self.Core.AddModule(ModuleType);
  Check(Assigned(Module),
        'AddModule didn''t return anything');
  CheckEquals(ModuleType.ClassName,
              Module.ClassName,
              'AddModule returned an unexpected module');

  Module := Self.Core.AddModule(ModuleType);
  Check(Assigned(Module),
        'AddModule didn''t return anything for an already-installed module');
end;

procedure TestTIdSipAbstractCore.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);

      Self.ReceiveInvite;

      Check(L1.Changed and L2.Changed, 'Not all Listeners notified, hence not added');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAllowedExtensionsCollectsExtensionsFromInstalledModules;
var
  I:                  Integer;
  ExpectedExtensions: TStrings;
  Mock:               TIdSipMultipleExtensionMessageModule;
  ReceivedExtensions: TStrings;
begin
  Mock := TIdSipMultipleExtensionMessageModule.Create(Self.Core);
  try
    ExpectedExtensions := TStringList.Create;
    try
      ExpectedExtensions.CommaText := Mock.AllowedExtensions;

      Self.Core.AddModule(TIdSipMessageModuleClass(Mock.ClassType));

      ReceivedExtensions := TStringList.Create;
      try
        for I := 0 to ReceivedExtensions.Count - 1 do
          CheckNotEquals(-1,
                         ExpectedExtensions.IndexOf(ReceivedExtensions[I]),
                         'Core doesn''t support extension "' + ReceivedExtensions[I] + '"');
      finally
        ReceivedExtensions.Free;
      end;
    finally
      ExpectedExtensions.Free;
    end;
  finally
    Mock.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestAllowedExtensionsRemovesDuplicateExtensions;
var
  ExpectedExtensions: TStringList;
begin
  ExpectedExtensions := TStringList.Create;
  try
    ExpectedExtensions.Duplicates := dupError;
    ExpectedExtensions.Sorted     := true;

    try
      ExpectedExtensions.CommaText := Self.Core.AllowedExtensions;
    except
      on EStringListError do
        Fail('AllowedExtensions didn''t remove duplicate extension declarations');
    end;
  finally
    ExpectedExtensions.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithDangerousHeaders;
const
  ViaHeader = 'SIP/2.0/TCP nether.hells.com';
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Value := Self.Destination.Address.Uri + '?'
                          + 'Record-Route=' + TIdSipUri.ParameterEncode('<sip:127.0.0.1>') + '&'
                          + 'Via=' + TIdSipUri.ParameterEncode(ViaHeader);

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(not Invite.HasHeader(RecordRouteHeader),
          'Record-Route header erroneously added');
    Check(Invite.Path.Count = 1,
          'Via header erroneously added');
    CheckNotEquals(ViaHeader,
                   Invite.LastHop.Value,
                   'Via header in URI used, not that of the creating UA');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithDifferentMethod;
begin
  Self.Destination.Address.Method := MethodInvite;
  try
    Self.Core.CreateRequest(MethodRegister, Self.Destination);
    Fail('Failed to bail out of a contradictory request');
  except
    on EIdSipTransactionUser do;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithFalseAdvertising;
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Value := Self.Destination.Value + '?'
                          + 'Accept=' + TIdSipUri.ParameterEncode('text/plain') + '&'
                          + 'Accept-Encoding=foo' + '&'
                          + 'Accept-Language=zh' + '&'
                          + 'Allow=bar' + '&'
                          + 'Contact=' + TIdSipUri.ParameterEncode('sip:foo@bar.com') + '&'
                          + 'Organization=orangutan' + '&'
                          + 'Supported=refer' + '&'
                          + 'User-Agent=nothing';

  Invite := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Check(not Invite.HasHeader(AcceptHeader),
          'Accept header erroneously added');
    Check(not Invite.HasHeader(AcceptEncodingHeader),
          'Accept-Encoding header erroneously added');
    Check(not Invite.HasHeader(AcceptLanguageHeader),
          'Accept-Language header erroneously added');
    Check(not Invite.HasHeader(AllowHeader),
          'Allow header erroneously added');
    Check(Invite.ContactCount = 1,
          'Contact header erroneously added');
    CheckEquals(Self.Core.Contact.AsString,
                Invite.FirstContact.AsString,
                'URI header used instead of UA''s');
    Check(not Invite.HasHeader(OrganizationHeader),
          'Organization header erroneously added');
    CheckEquals(Self.Core.AllowedExtensions,
                Invite.Supported.Value,
          'Supported header added/used instead of UA''s');
    CheckEquals(Self.Core.UserAgentName,
                Invite.FirstHeader(UserAgentHeader).Value,
                'User-Agent header added/used instead of UA''s');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithHeaders;
const
  Replaces = '1;from-tag=2;to-tag=3';
  Subject  = 'nothing important';
var
  Invite: TIdSipRequest;
  URI:    String;
begin
  URI := Self.Destination.Address.Uri;

  Self.Destination.Value := URI + '?'
                    + 'Replaces=' + TIdSipUri.ParameterEncode(Replaces) + '&'
                    + 'Subject=' + TIdSipUri.ParameterEncode(Subject);

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(Invite.RequestUri.Headers.IsEmpty,
          'The Request-URI must have no headers');
    CheckEquals(URI,
                Invite.RequestUri.AsString,
                'Request-URI');
    Check(Invite.HasReplaces,
          'No Replaces header added');
    CheckEquals(Replaces,
                Invite.Replaces.FullValue,
                'Incorrect Replaces header value');
    Check(Invite.HasHeader(SubjectHeaderFull),
          'No Subject header added');
    CheckEquals(Subject,
                Invite.Subject.FullValue,
                'Incorrect Subject header value');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithKnownParams;
const
  MaddrValue     = '224.0.0.1';
  TransportValue = 'tcp';
  TtlValue       = '1';
  UserValue      = 'foo';
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Address.Username := '';
  Self.Destination.Address.AddParameter(MaddrParam, MaddrValue);
  Self.Destination.Address.AddParameter(TransportParam, TransportValue);
  Self.Destination.Address.AddParameter(TtlParam, TtlValue);
  Self.Destination.Address.AddParameter(UserParam, UserValue);

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(Invite.RequestUri.HasParameter(MaddrParam),
          'Request-URI''s missing the maddr parameter');
    CheckEquals(MaddrValue,
                Invite.RequestUri.ParamValue(MaddrParam),
                'Request-URI has an incorrect maddr parameter');
    Check(Invite.RequestUri.HasParameter(TransportParam),
          'Request-URI''s missing the transport parameter');
    CheckEquals(TransportValue,
                Invite.RequestUri.ParamValue(TransportParam),
                'Request-URI has an incorrect Transport parameter');
    Check(Invite.RequestUri.HasParameter(TtlParam),
          'Request-URI''s missing the ttl parameter');
    CheckEquals(TtlValue,
                Invite.RequestUri.ParamValue(TtlParam),
                'Request-URI has an incorrect Ttl parameter');
    Check(Invite.RequestUri.HasParameter(UserParam),
          'Request-URI''s missing the user parameter');
    CheckEquals(UserValue,
                Invite.RequestUri.ParamValue(UserParam),
                'Request-URI has an incorrect User parameter');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithMalformedHeader;
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Address.Headers.Add(ReplacesHeader).Value := '1';

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(Invite.IsMalformed,
          'Request not marked as malformed despite a malformed Replaces header');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithMethod;
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Address.Method := MethodInvite;

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    CheckEquals(Self.Destination.Address.Method,
                Invite.Method,
                'URI''s method parameter not used');
    Check(not Invite.RequestUri.HasMethod,
          'Request-URI''s method parameter not removed');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestFromUriWithUnknownParam;
const
  FooParam      = 'foo';
  FooParamValue = 'bar';
var
  Invite: TIdSipRequest;
begin
  Self.Destination.Address.AddParameter(FooParam, FooParamValue);

  Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(Invite.RequestUri.HasParameter(FooParam),
          'Request-URI doesn''t have the unknown parameter');
    CheckEquals(FooParamValue,
                Invite.RequestUri.ParamValue(FooParam),
                'Request-URI has wrong value for unknown parameter');
  finally
    Invite.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestCreateRequestWithGruu;
var
  Request: TIdSipRequest;
begin
  Self.Core.UseGruu := true;
  Self.Core.Gruu := Self.Core.Contact;
  Self.Core.Gruu.Address.Host := Self.Core.Gruu.Address.Host + '.com';

  Request := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Check(Request.HasHeader(SupportedHeaderFull),
          'Request has no Supported header');
    Check(Request.SupportsExtension(ExtensionGruu),
          'Supported header doesn''t indicate that the UA supports "gruu"');
    CheckEquals(Self.Core.Gruu.Address.Host,
                Request.FirstContact.Address.Host,
                'Contact MUST contain GRUU');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestHasUnknownContentEncoding;
begin
  Self.Invite.Headers.Remove(Self.Invite.FirstHeader(ContentEncodingHeaderFull));

  Check(not Self.Core.HasUnknownContentEncoding(Self.Invite),
        'Vacuously true');

  Self.Invite.AddHeader(ContentEncodingHeaderFull);
  Check(Self.Core.HasUnknownContentEncoding(Self.Invite),
        'No encodings are supported');
end;

procedure TestTIdSipAbstractCore.TestHasUnsupportedExtension;
var
  InviteExtensions: TStrings;
begin
  Check(not Self.Invite.HasHeader(SupportedHeaderFull),
        'Sanity check: the INVITE shouldn''t have a Require header at the '
      + 'beginning of the test');
  Check(not Self.Core.HasUnsupportedExtension(Self.Invite),
        'Request with no Supported header can''t claim to support an unsupported extension');
  Check(not Self.Invite.HasHeader(SupportedHeaderFull),
        'HasUnsupportedExtension added a Supported header to the request');

  Self.Invite.Supported.Value := 'unknown-extension';
  Check(Self.Core.HasUnsupportedExtension(Self.Invite),
        'Core somehow supports the "' + Self.Invite.Supported.Value + '" extension');

  InviteExtensions := TStringList.Create;
  try
    InviteExtensions.CommaText := Self.Core.InviteModule.AllowedExtensions;

    Check(InviteExtensions.Count > 0,
          'Sanity check: an InviteModule should support at least one '
        + 'extension, namely "replaces"');
    Self.Invite.Supported.Value := InviteExtensions[0];
  Check(not Self.Core.HasUnsupportedExtension(Self.Invite),
        'Core somehow doesn''t support the ' + InviteExtensions[0] + ' extension');
  finally
    InviteExtensions.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestIsMethodSupported;
begin
  Check(not Self.Core.IsMethodSupported(MethodRegister),
        MethodRegister + ' not allowed');

  Self.Core.AddModule(TIdSipRegisterModule);
  Check(Self.Core.IsMethodSupported(MethodRegister),
        MethodRegister + ' not recognised as an allowed method');

  Check(not Self.Core.IsMethodSupported(' '),
        ''' '' recognised as an allowed method');
end;

procedure TestTIdSipAbstractCore.TestIsSchemeAllowed;
begin
  Check(not Self.Core.IsMethodSupported(SipScheme),
        SipScheme + ' not allowed');

  Self.Core.AddAllowedScheme(SipScheme);
  Check(Self.Core.IsSchemeAllowed(SipScheme),
        SipScheme + ' not recognised as an allowed scheme');

  Check(not Self.Core.IsSchemeAllowed(' '),
        ''' '' not recognised as an allowed scheme');
end;

procedure TestTIdSipAbstractCore.TestLoopDetection;
var
  Response: TIdSipResponse;
begin
  // cf. RFC 3261, section 8.2.2.2
  Self.Dispatcher.AddServerTransaction(Self.Invite, Self.Dispatcher.Transport);

  // wipe out the tag & give a different branch
  Self.Invite.ToHeader.Value := Self.Invite.ToHeader.Address.URI;
  Self.Invite.LastHop.Branch := Self.Invite.LastHop.Branch + '1';

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPLoopDetected, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipAbstractCore.TestModuleForString;
begin
  CheckEquals(TIdSipNullModule.ClassName,
              Self.Core.ModuleFor('').ClassName,
              'Empty string');
  CheckEquals(TIdSipNullModule.ClassName,
              Self.Core.ModuleFor(MethodRegister).ClassName,
              MethodRegister + ' but no module added');

  Self.Core.AddModule(TIdSipRegisterModule);
  CheckNotNull(Self.Core.ModuleFor(MethodRegister),
               MethodRegister + ' but no module added');
  CheckEquals(TIdSipRegisterModule.ClassName,
              Self.Core.ModuleFor(MethodRegister).ClassName,
              MethodRegister + ' after module added: wrong type');
  CheckEquals(TIdSipNullModule.ClassName,
              Self.Core.ModuleFor(Lowercase(MethodRegister)).ClassName,
              Lowercase(MethodRegister)
            + ': RFC 3261 defines REGISTER''s method as "REGISTER"');
end;

procedure TestTIdSipAbstractCore.TestModuleForClassType;
begin
  Check(Assigned(Self.Core.ModuleFor(TIdSipDoubleExtensionMessageModule)),
        'ModuleFor(<uninstalled module>) returned a nil pointer');
  CheckEquals(TIdSipNullModule.ClassName,
              Self.Core.ModuleFor(TIdSipDoubleExtensionMessageModule).ClassName,
              'ModuleFor(<uninstalled module>) didn''t return the null module');

  Self.Core.AddModule(TIdSipDoubleExtensionMessageModule);
  CheckEquals(TIdSipDoubleExtensionMessageModule.ClassName,
              Self.Core.ModuleFor(TIdSipDoubleExtensionMessageModule).ClassName,
              'ModuleFor(<installed module>) didn''t return the installed module');
end;

procedure TestTIdSipAbstractCore.TestNextCallID;
var
  CallID: String;
begin
  CallID := Self.Core.NextCallID;

  Fetch(CallID, '@');

  CheckEquals(Self.Core.HostName, CallID, 'HostName not used');
end;

procedure TestTIdSipAbstractCore.TestNextTag;
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

procedure TestTIdSipAbstractCore.TestNotifyOfChange;
var
  C: TIdSipCoreWithExposedNotify;
  O: TIdObserverListener;
begin
  C := TIdSipCoreWithExposedNotify.Create;
  try
    O := TIdObserverListener.Create;
    try
      C.AddObserver(O);
      C.TriggerNotify;
      Check(O.Changed,
            'Observer not notified');
      Check(O.Data = C,
           'Core didn''t return itself as parameter in the notify');
    finally
      O.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestRejectUnknownContentEncoding;
var
  Response: TIdSipResponse;
begin
  Self.Invite.FirstHeader(ContentTypeHeaderFull).Value := SdpMimeType;

  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(ContentEncodingHeaderFull).Value := 'gzip';

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptEncodingHeader), 'No Accept-Encoding header');
  CheckEquals('',
              Response.FirstHeader(AcceptEncodingHeader).Value,
              'Accept value');
end;

procedure TestTIdSipAbstractCore.TestRejectUnknownContentLanguage;
var
  Response: TIdSipResponse;
begin
  Self.Core.AddAllowedLanguage('fr');

  Self.Invite.AddHeader(ContentLanguageHeader).Value := 'en_GB';

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedMediaType, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(AcceptLanguageHeader), 'No Accept-Language header');
  CheckEquals(Self.Core.AllowedLanguages,
              Response.FirstHeader(AcceptLanguageHeader).Value,
              'Accept-Language value');
end;

procedure TestTIdSipAbstractCore.TestRejectUnknownExtension;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.AddHeader(RequireHeader).Value := '100rel';

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPBadExtension, Response.StatusCode, 'Status-Code');
  Check(Response.HasHeader(UnsupportedHeader), 'No Unsupported header');
  CheckEquals(Self.Invite.FirstHeader(RequireHeader).Value,
              Response.FirstHeader(UnsupportedHeader).Value,
              'Unexpected Unsupported header value');
end;

procedure TestTIdSipAbstractCore.TestRejectUnknownScheme;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;

  Self.Invite.RequestUri.URI := 'tel://1';
  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPUnsupportedURIScheme, Response.StatusCode, 'Status-Code');
end;

procedure TestTIdSipAbstractCore.TestRejectUnsupportedMethod;
var
  Response: TIdSipResponse;
begin
  Self.Invite.Method := MethodRegister;
  Self.Invite.CSeq.Method := Self.Invite.Method;

  Self.MarkSentResponseCount;

  Self.ReceiveInvite;

  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPNotImplemented,
              Response.StatusCode,
              'Unexpected response');
  Check(Response.HasHeader(AllowHeader),
        'Allow header is mandatory. cf. RFC 3261 section 8.2.1');

  CheckCommaSeparatedHeaders(Self.Core.KnownMethods,
                             Response.FirstHeader(AllowHeader),
                             'Allow header');
end;

procedure TestTIdSipAbstractCore.TestRejectUnsupportedSipVersion;
var
  Response: TIdSipResponse;
begin
  Self.MarkSentResponseCount;
  Self.Invite.SIPVersion := 'SIP/1.0';

  Self.ReceiveInvite;

  // You would think that we would check that both a 100 Trying and an error
  // response would be sent. However, 100 Tryings are sent by
  // TIdSipInboundInvites, and the UA core rejects the message before that
  // response would be generated.
  CheckResponseSent('No response sent');

  Response := Self.LastSentResponse;
  CheckEquals(SIPSIPVersionNotSupported,
              Response.StatusCode,
              'Status-Code');
end;

procedure TestTIdSipAbstractCore.TestRemoveObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Core.AddObserver(L1);
      Self.Core.AddObserver(L2);
      Self.Core.RemoveObserver(L2);

      Self.ReceiveInvite;

      Check(L1.Changed and not L2.Changed,
            'Listener notified, hence not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestRequiresUnsupportedExtension;
var
  InviteExtensions: TStrings;
begin
  Check(not Self.Invite.HasHeader(RequireHeader),
        'Sanity check: the INVITE shouldn''t have a Require header at the '
      + 'beginning of the test');
  Check(not Self.Core.RequiresUnsupportedExtension(Self.Invite),
        'Request with no Require header can''t require an unsupported extension');
  Check(not Self.Invite.HasHeader(RequireHeader),
        'RequiresUnsupportedExtension added a Require header to the request');

  Self.Invite.Require.Value := 'unknown-extension';
  Check(Self.Core.RequiresUnsupportedExtension(Self.Invite),
        'Core somehow supports the ' + Self.Invite.Require.Value + ' extension');

  InviteExtensions := TStringList.Create;
  try
    InviteExtensions.CommaText := Self.Core.InviteModule.AllowedExtensions;

    Check(InviteExtensions.Count > 0,
          'Sanity check: an InviteModule should support at least one '
        + 'extension, namely "replaces"');
    Self.Invite.Require.Value := InviteExtensions[0];
  Check(not Self.Core.RequiresUnsupportedExtension(Self.Invite),
        'Core somehow doesn''t support the ' + InviteExtensions[0] + ' extension');
  finally
    InviteExtensions.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestScheduleEvent;
var
  EventCount: Integer;
begin
  EventCount := Self.DebugTimer.EventCount;
  Self.Core.ScheduleEvent(Self.ScheduledEvent, 50, Self.Invite.Copy);
  Check(EventCount < DebugTimer.EventCount,
        'Event not scheduled');
end;

procedure TestTIdSipAbstractCore.TestSendRequest;
var
  Dest:   TIdSipLocation;
  Invite: TIdSipRequest;
begin
  Dest := TIdSipLocation.Create('TCP', '127.0.0.2', 5060);
  try
    Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
    try
      Self.MarkSentRequestCount;
      Self.Core.SendRequest(Invite, Dest);
      CheckRequestSent('No request sent');
      Check(Self.LastSentRequest.Equals(Invite),
            'Sent message differs from the message at the network layer');
    finally
      Invite.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendRequestMalformedRequest;
var
  Dest:   TIdSipLocation;
  Invite: TIdSipRequest;
begin
  Dest := TIdSipLocation.Create('TCP', '127.0.0.2', 5060);
  try
    Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
    try
      Invite.CallID := '@@illegal';
      try
        Self.Core.SendRequest(Invite, Dest);

        Fail('Failed to bail out on sending a malformed request');
      except
        on EIdSipTransactionUser do;
      end;
    finally
      Invite.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendRequestUnknownMethod;
var
  Dest:   TIdSipLocation;
  Invite: TIdSipRequest;
begin
  Dest := TIdSipLocation.Create('TCP', '127.0.0.2', 5060);
  try
    Invite := Self.Core.CreateRequest('UNKNOWN', Self.Destination);
    try
      try
        Self.Core.SendRequest(Invite, Dest);

        Fail('Failed to bail out on sending a request of unknown method');
      except
        on EIdSipTransactionUser do;
      end;
    finally
      Invite.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendRequestUnknownRequiredExtension;
var
  Dest:   TIdSipLocation;
  Invite: TIdSipRequest;
begin
  Dest := TIdSipLocation.Create('TCP', '127.0.0.2', 5060);
  try
    Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
    try
      Invite.Require.Value := 'unknown-extension';

      try
        Self.Core.SendRequest(Invite, Dest);

        Fail('Failed to bail out on sending a request that purports to '
           + 'require something we don''t support');
      except
        on EIdSipTransactionUser do;
      end;
    finally
      Invite.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendRequestUnknownSupportedExtension;
var
  Dest:   TIdSipLocation;
  Invite: TIdSipRequest;
begin
  Dest := TIdSipLocation.Create('TCP', '127.0.0.2', 5060);
  try
    Invite := Self.Core.CreateRequest(MethodInvite, Self.Destination);
    try
      Invite.Supported.Value := 'unknown-extension';

      try
        Self.Core.SendRequest(Invite, Dest);

        Fail('Failed to bail out on sending a request that purports to '
           + 'support something we don''t');
      except
        on EIdSipTransactionUser do;
      end;
    finally
      Invite.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendResponse;
var
  OK: TIdSipResponse;
begin
  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    Self.MarkSentResponseCount;
    Self.Core.SendResponse(OK);
    CheckResponseSent('No response sent');
    Check(Self.LastSentResponse.Equals(OK),
          'Sent message differs from the message at the network layer');
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendResponseMalformedResponse;
var
  OK: TIdSipResponse;
begin
  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    OK.CallID := '@@illegal';

    try
      Self.Core.SendResponse(OK);
      Fail('Failed to bail on sending a malformed response');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSendResponseUnknownSupportedExtension;
var
  OK: TIdSipResponse;
begin
  OK := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    OK.Supported.Value := 'unknown-extension';

    try
      Self.Core.SendResponse(OK);
      Fail('Failed to bail on sending a response claiming to support something '
         + 'we don''t');
    except
      on EIdSipTransactionUser do;
    end;
  finally
    OK.Free;
  end;
end;

procedure TestTIdSipAbstractCore.TestSetInstanceID;
const
  ValidUrn = 'urn:uuid:00000000-0000-0000-0000-000000000000';
begin
  try
    Self.Core.InstanceID := 'foo';
    Fail('Failed to bail on setting InstanceID with something other than a URN');
  except
    on EIdSipTransactionUser do;
  end;

  Self.Core.InstanceID := ValidUrn;
  CheckEquals(ValidUrn,
              Self.Core.InstanceID,
              'InstanceID');
end;

procedure TestTIdSipAbstractCore.TestUseGruuAffectsAllowedExtensions;
begin
  Check(Pos(ExtensionGruu, Self.Core.AllowedExtensions) = 0,
        'By default, the UA shouldn''t support GRUU');

  Self.Core.UseGruu := true;
  Check(Pos(ExtensionGruu, Self.Core.AllowedExtensions) > 0,
        'Setting UseGruu didn''t affect AllowedExtensions');
end;

//******************************************************************************
//* TIdSipNullAction                                                           *
//******************************************************************************
//* TIdSipNullAction Public methods ********************************************

function TIdSipNullAction.Method: String;
begin
  Result := '';
end;

//* TIdSipNullAction Protected methods *****************************************

function TIdSipNullAction.CreateNewAttempt: TIdSipRequest;
begin
  Result := nil;
end;

//******************************************************************************
//* TestTIdSipActions                                                          *
//******************************************************************************
//* TestTIdSipActions Public methods *******************************************

procedure TestTIdSipActions.SetUp;
begin
  inherited SetUp;

  Self.Actions := TIdSipActions.Create;
  Self.Options := TIdSipRequest.Create;
  Self.Options.Assign(Self.Invite);
  Self.Options.Method := MethodOptions;

  Self.ActionProcUsed      := '';
  Self.DidntFindActionName := 'DidntFindAction';
  Self.FoundActionName     := 'FoundActionName';
end;

procedure TestTIdSipActions.TearDown;
begin
  Self.Options.Free;
  Self.Actions.Free;

  inherited TearDown;
end;

//* TestTIdSipActions Published methods ****************************************

procedure TestTIdSipActions.TestActionCount;
var
  I: Integer;
begin
  for I := 1 to 5 do begin
    Self.Actions.Add(TIdSipNullAction.Create(Self.Core));
    CheckEquals(I, Self.Actions.Count, 'Action not added');
  end;
end;

procedure TestTIdSipActions.TestAddActionNotifiesObservers;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(L1);

    Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

    Check(L1.Changed, 'TIdSipInboundInvite didn''t notify listener');
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestAddObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);

      Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

      Check(L1.Changed, 'TIdSipInboundInvite didn''t notify L1, thus didn''t add it');
      Check(L2.Changed, 'TIdSipInboundInvite didn''t notify L2, thus didn''t add it');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestCleanOutTerminatedActions;
var
  A:           TIdSipAction;
  ActionCount: Integer;
  O:           TIdObserverListener;
begin
  A := TIdSipNullAction.Create(Self.Core);
  Self.Actions.Add(A);

  ActionCount := Self.Actions.Count;
  A.Terminate;

  O := TIdObserverListener.Create;
  try
    Self.Actions.AddObserver(O);

    Self.Actions.CleanOutTerminatedActions;

    Check(Self.Actions.Count < ActionCount,
          'Terminated action not destroyed');
    Check(O.Changed, 'Observers not notified of change');
  finally
    Self.Actions.RemoveObserver(O);
    O.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionForGruu;
const
  LocalGruuOne = 'sip:127.0.0.1;grid=gruu-one';
  LocalGruuTwo = 'sip:127.0.0.1;grid=gruu-two';
var
  A: TIdSipAction;
  B: TIdSipAction;
begin
  // Set us up to use GRUU
  Self.Core.UseGruu := true;
  // We have to set this because the InboundSession will Ring, which creates a
  // response with a Contact defined by Self.Core.Gruu.
  Self.Core.Gruu.Value := 'sip:some.gruu';
  Self.Invite.Supported.Values.Add(ExtensionGruu);

  // Create two actions (which will use different LocalGruus)
  A := Self.Actions.Add(TIdSipInboundSession.CreateInbound(Self.Core, Self.Invite, false));
  A.LocalGruu.Value := '<' + LocalGruuOne + '>';
  B := Self.Actions.Add(TIdSipOutboundSession.Create(Self.Core));
  B.LocalGruu.Value := '<' + LocalGruuTwo + '>';

  CheckEquals(LocalGruuOne,
              Self.Actions.FindActionForGruu(LocalGruuOne).LocalGruu.Address.AsString,
             'LocalGruuOne');
  CheckEquals(LocalGruuTwo,
              Self.Actions.FindActionForGruu(LocalGruuTwo).LocalGruu.Address.AsString,
              'LocalGruuTwo');
end;

procedure TestTIdSipActions.TestFindActionForGruuNoActions;
begin
  Check(not Assigned(Self.Actions.FindActionForGruu('sip:127.0.0.1')),
        'Action found despite there being no actions');

  Self.Actions.Add(TIdSipInboundSession.CreateInbound(Self.Core, Self.Invite, false));
  Check(not Assigned(Self.Actions.FindActionForGruu('sip:127.0.0.1')),
        'Action found despite there being no matching action');
end;

procedure TestTIdSipActions.TestFindActionForGruuWithOwnedActions;
const
  LocalGruu = 'sip:127.0.0.1;grid=gruu';
var
  A: TIdSipAction;
  B: TIdSipAction;
begin
  // Set us up to use GRUU
  Self.Core.UseGruu := true;
  Self.Invite.Supported.Values.Add(ExtensionGruu);

  // Create two actions (which will use different LocalGruus
  A := Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  A.LocalGruu.Value := '<' + LocalGruu + '>';
  B := Self.Actions.Add(TIdSipInboundSession.CreateInbound(Self.Core, Self.Invite, false));
  B.LocalGruu.Value := '<' + LocalGruu + '>';

  Check(B = Self.Actions.FindActionForGruu(LocalGruu),
        'LocalGruu');
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
begin
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  A := Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(A.ID, Finder);

    Check(Finder.Action = A, 'Wrong action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoActions;
var
  Finder: TIdSipActionFinder;
begin
  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform('', Finder);

    Check(not Assigned(Finder.Action), 'An action found in an empty list');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformBlockNoMatch;
var
  Action:    TIdSipAction;
  AnotherID: String;
  Finder:    TIdSipActionFinder;
begin
  Action := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false);
  Self.Actions.Add(Action);

  AnotherID := Action.ID + '1';

  Finder := TIdSipActionFinder.Create;
  try
    Self.Actions.FindActionAndPerform(AnotherID, Finder);

    Check(not Assigned(Finder.Action), 'An action found');
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlock;
var
  A:      TIdSipAction;
  Finder: TIdSipActionFinder;
  Switch: TIdSipActionSwitch;
begin
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  A := Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  Self.Actions.Add(TIdSipOutboundOptions.Create(Self.Core));

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(A.ID,
                                          Finder,
                                          Switch);

      Check(Assigned(Finder.Action), 'Didn''t find action');
      Check(not Switch.Executed, 'Alternative block executed');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestFindActionAndPerformOrBlockNoMatch;
var
  Action:    TIdSipAction;
  AnotherID: String;
  Finder:    TIdSipActionFinder;
  Switch:    TIdSipActionSwitch;
begin
  Action := TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false);
  Self.Actions.Add(Action);

  AnotherID := Action.ID + '1';

  Finder := TIdSipActionFinder.Create;
  try
    Switch := TIdSipActionSwitch.Create;
    try
      Self.Actions.FindActionAndPerformOr(AnotherID,
                                          Finder,
                                          Switch);

      Check(not Assigned(Finder.Action), 'Found action');
      Check(Switch.Executed, 'Alternative block didn''t execute');
    finally
      Switch.Free;
    end;
  finally
    Finder.Free;
  end;
end;

procedure TestTIdSipActions.TestInviteCount;
begin
  CheckEquals(0, Self.Actions.InviteCount, 'No messages received');

  Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE');

  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  CheckEquals(1, Self.Actions.InviteCount, 'One INVITE, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundInvite.Create(Self.Core));
  CheckEquals(2, Self.Actions.InviteCount, 'Two INVITEs, one OPTIONS');

  Self.Actions.Add(TIdSipOutboundSession.Create(Self.Core));
  CheckEquals(2,
              Self.Actions.InviteCount,
              'Two INVITEs, one OPTIONS, and a Session');
end;

procedure TestTIdSipActions.TestRemoveObserver;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Actions.AddObserver(L1);
      Self.Actions.AddObserver(L2);
      Self.Actions.RemoveObserver(L1);

      Self.Actions.Add(TIdSipInboundInvite.CreateInbound(Self.Core, Self.Invite, false));

      Check(not L1.Changed, 'L1 notified, thus not removed');
      Check(L2.Changed, 'L2 not notified, thus not added');
    finally
      Self.Actions.RemoveObserver(L2);
      L2.Free;
    end;
  finally
    Self.Actions.RemoveObserver(L1);
    L1.Free;
  end;
end;

procedure TestTIdSipActions.TestTerminateAllActions;
begin
  // We don't add INVITEs here because INVITEs need additional events to
  // properly terminate: an INVITE needs to wait for a final response, etc.
  Self.Actions.Add(TIdSipInboundOptions.CreateInbound(Self.Core, Self.Options, false));
  Self.Actions.Add(TIdSipOutboundRegistration.Create(Self.Core));

  Self.Actions.TerminateAllActions;
  Self.Actions.CleanOutTerminatedActions;
  CheckEquals(0,
              Self.Actions.Count,
              'Actions container didn''t terminate all actions');
end;

//******************************************************************************
//* TestLocation                                                               *
//******************************************************************************
//* TestLocation Public methods ************************************************

procedure TestLocation.SetUp;
begin
  inherited SetUp;

  Self.InviteMimeType := '';
  Self.InviteOffer    := '';
  Self.NetworkFailure := false;
  Self.TransportParam := SctpTransport;
end;

//* TestLocation Private methods ***********************************************

function TestLocation.CreateAction: TIdSipOutboundInitialInvite;
begin
  Result := Self.Core.AddOutboundAction(TIdSipOutboundInitialInvite) as TIdSipOutboundInitialInvite;
  Result.Destination := Self.Destination;
  Result.MimeType    := Self.InviteMimeType;
  Result.Offer       := Self.InviteOffer;
  Result.AddActionListener(Self);
  Result.AddInviteListener(Self);
  Result.Send;
end;

procedure TestLocation.OnAuthenticationChallenge(Action: TIdSipAction;
                                                 Response: TIdSipResponse);
begin
end;

procedure TestLocation.OnCallProgress(InviteAgent: TIdSipOutboundInvite;
                                      Response: TIdSipResponse);
begin
end;

procedure TestLocation.OnFailure(Action: TIdSipAction;
                                 Response: TIdSipResponse;
                                 const Reason: String);
begin
end;

procedure TestLocation.OnDialogEstablished(InviteAgent: TIdSipOutboundInvite;
                                           NewDialog: TIdSipDialog);
begin
end;

procedure TestLocation.OnNetworkFailure(Action: TIdSipAction;
                                        ErrorCode: Cardinal;
                                        const Reason: String);
begin
  Self.NetworkFailure := true;
end;

procedure TestLocation.OnRedirect(Action: TIdSipAction;
                                  Redirect: TIdSipResponse);
begin
end;

procedure TestLocation.OnSuccess(Action: TIdSipAction;
                                 Msg: TIdSipMessage);
begin
end;

//* TestLocation Published methods *********************************************

procedure TestLocation.TestAllLocationsFail;
var
  I:         Integer;
  Locations: TIdSipLocations;
begin
  // SRV records point to Self.Destination.Address.Host;
  // Self.Destination.Address.Host resolves to two name records.

  Self.Locator.AddSRV(Self.Destination.Address.Host,
                      SrvUdpPrefix,
                      0,
                      0,
                      5060,
                      Self.Destination.Address.Host);
  Self.Locator.AddA   (Self.Destination.Address.Host, '127.0.0.2');
  Self.Locator.AddAAAA(Self.Destination.Address.Host, '::2');

  Locations := TIdSipLocations.Create;
  try
    Self.Locator.FindServersFor(Self.Destination.Address, Locations);

    Self.MarkSentRequestCount;
    Self.CreateAction;
    CheckRequestSent('No request sent');
    for I := 0 to Locations.Count - 1 do begin
      // This should trigger a resend of the message to a new location.
      Self.Dispatcher.Transport.FireOnException(Self.LastSentRequest,
                                                EIdConnectException,
                                                '10061',
                                                'Connection refused');
    end;

    // Locations.Count >= 0, so the typecast is safe.
    CheckEquals(Self.RequestCount + Cardinal(Locations.Count),
                Self.SentRequestCount,
                'Number of requests sent');
  finally
    Locations.Free;
  end;

  Check(Self.NetworkFailure,
        'No notification of failure after all locations attempted');
end;

procedure TestLocation.TestLooseRoutingProxy;
const
  ProxyAAAARecord = '::1';
  ProxyHost       = 'gw1.leo-ix.net';
  ProxyTransport  = SctpTransport;
  ProxyUri        = 'sip:' + ProxyHost + ';lr';
var
  RequestUriTransport: String;
begin
  RequestUriTransport := Self.Invite.LastHop.Transport;

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  // Set so LastSentRequest uses the correct transport
  Self.Dispatcher.TransportType := ProxyTransport;

  Self.Locator.AddSRV(ProxyHost, SrvSctpPrefix, 0, 0, 5060, ProxyHost);
  Self.Locator.AddAAAA(ProxyHost, ProxyAAAARecord);

  Self.Locator.AddSRV(Self.Destination.Address.Host, SrvTcpPrefix, 0, 0,
                      5060, Self.Destination.Address.Host);

  Self.Locator.AddA(Self.Destination.Address.Host, '127.0.0.1');

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(ProxyTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestStrictRoutingProxy;
const
  ProxyUri = 'sip:127.0.0.1;transport=' + TransportParamSCTP;
var
  RequestUriTransport: String;
begin
  // The transport specified in the INVITE's Request-URI differs from that in
  // the Route header. We check that the message goes over the transport
  // specified in the Request-URI header, as per RFC 3261, section 8.1.2

  Self.Destination.Address.Transport := TransportParamTCP;
  RequestUriTransport                := TcpTransport;  

  Self.Core.Proxy.Uri := ProxyUri;
  Self.Core.HasProxy  := true;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  CheckEquals(RequestUriTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Wrong transport means UA gave Locator wrong URI');
end;

procedure TestLocation.TestUseCorrectTransport;
const
  CorrectTransport = SctpTransport;
var
  Action: TIdSipAction;
  Domain: String;
begin
  Domain := Self.Destination.Address.Host;

  Self.Dispatcher.TransportType := SctpTransport;  
  // NAPTR record points to SCTP SRV record whose target resolves to the A
  // record.
  Self.Locator.AddNAPTR(Domain, 0, 0, NaptrDefaultFlags, NaptrSctpService, SrvSctpPrefix + Domain);
  Self.Locator.AddSRV(Domain, SrvSctpPrefix, 0, 0, 5060, Domain);
  Self.Locator.AddSRV(Domain, SrvTcpPrefix,  1, 0, 5060, Domain);

  Self.MarkSentRequestCount;
  Action := Self.CreateAction;

  CheckRequestSent('No request sent');
  CheckEquals(CorrectTransport,
              Self.LastSentRequest.LastHop.Transport,
              'Incorrect transport');
  Check(Self.LastSentRequest.Equals(Action.InitialRequest),
        'Action''s InitialRequest not updated to the latest attempt');
end;

procedure TestLocation.TestUseTransportParam;
begin
  Self.Dispatcher.TransportType := Self.TransportParam;
  Self.Destination.Address.Transport := Self.TransportParam;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(SctpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use transport param');
end;

procedure TestLocation.TestUseUdpByDefault;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(UdpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use UDP by default');
end;

procedure TestLocation.TestVeryLargeMessagesUseAReliableTransport;
begin
  Self.InviteOffer    := TIdSipTestResources.VeryLargeSDP('localhost');
  Self.InviteMimeType := SdpMimeType;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  Self.CheckRequestSent('No request sent');

  CheckEquals(TcpTransport,
              Self.LastSentRequest.LastHop.Transport,
              'INVITE didn''t use a reliable transport despite the large size '
            + 'of the message');
end;

//******************************************************************************
//* TestTIdSipMessageModule                                                    *
//******************************************************************************
//* TestTIdSipMessageModule Public methods *************************************

procedure TestTIdSipMessageModule.SetUp;
begin
  inherited SetUp;

  // Since we're testing non-virtual methods, it doesn't matter which module
  // we choose.
  Self.Module := Self.Core.ModuleFor(MethodOptions);
end;

//* TestTIdSipMessageModule Published methods **********************************

procedure TestTIdSipMessageModule.TestAddAllowedContentType;
begin
  Self.Module.AddAllowedContentType(SdpMimeType);
  Self.Module.AddAllowedContentType(PlainTextMimeType);

  CheckEquals(2,
              Self.Module.AllowedContentTypes.Count,
              'Number of allowed Content-Types');

  CheckEquals(SdpMimeType,
              Self.Module.AllowedContentTypes[0],
              SdpMimeType);
  CheckEquals(PlainTextMimeType,
              Self.Module.AllowedContentTypes[1],
              PlainTextMimeType);
end;

procedure TestTIdSipMessageModule.TestAddAllowedContentTypeMalformed;
var
  ContentTypes: TStrings;
begin
  ContentTypes := TStringList.Create;
  try
    ContentTypes.AddStrings(Self.Module.AllowedContentTypes);

    Self.Module.AddAllowedContentType(' ');
    CheckEquals(ContentTypes.CommaText,
                Self.Module.AllowedContentTypes.CommaText,
                'Malformed Content-Type was allowed');
  finally
    ContentTypes.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestAddAllowedContentTypes;
var
  Actual:   TStrings;
  Expected: TStrings;
begin
  Expected := TStringList.Create;
  try
    Expected.Add(SdpMimeType);
    Expected.Add('message/sipfrag');

    Self.Module.AddAllowedContentTypes(Expected);

    Actual := Self.Module.AllowedContentTypes;

    CheckEquals(Expected.CommaText,
                Actual.CommaText,
                'Content types not added');
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptDoesntAffectRequestParam;
var
  Request: TIdSipRequest;
begin
  Request := TIdSipRequest.Create;
  try
    Check(not Request.HasHeader(AcceptHeader),
          'A newly-instantiated request shouldn''t have an Accept header');

    Self.Module.HasKnownAccept(Request);
    Check(not Request.HasHeader(AcceptHeader),
          'MessageModule.HasKnownAccept() has ADDED an Accept header');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptEmptyAcceptHeader;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.Accept.Value := '';

    // This is OK: a blank Accept header means "don't send me ANY bodies".
    Check(Self.Module.HasKnownAccept(Request),
          'A request with a blank Accept header contains, vacuously, no '
        + 'supported Content-Types');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptNoAcceptableMimeTypes;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.Accept.Value := 'x-application/unknown';

    Check(not Self.Module.HasKnownAccept(Request),
          'An unsupported MIME type');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptNoAcceptHeader;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.RemoveAllHeadersNamed(AcceptHeader);

    Check(Self.Module.HasKnownAccept(Request),
          'A request with no Accept header contains, as per RFC 3261 section '
        + '11.2, a supported Content-Type: ' + SdpMimeType);
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptWithKnownMimeType;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Request.Accept.Value := SdpMimeType;

    Check(Self.Module.HasKnownAccept(Request),
          'Request with ' + Request.Accept.AsString);
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasKnownAcceptWithKnownMimeTypeAmongOthers;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodInvite, Self.Destination);
  try
    Request.Accept.Value := 'text/plain, ' + SdpMimeType + ', message/sipfrag';

    Check(Self.Module.HasKnownAccept(Request),
          'Request with ' + Request.Accept.AsString);
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestHasUnknownContentType;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.RemoveAllHeadersNamed(ContentTypeHeaderFull);

    Check(not Self.Module.HasUnknownContentType(Request),
          'Vacuously true');

    Request.AddHeader(ContentTypeHeaderFull).Value := SdpMimeType;
    Check(not Self.Module.HasUnknownContentType(Request),
          SdpMimeType + ' MUST supported');

    Request.RemoveHeader(Request.FirstHeader(ContentTypeHeaderFull));
    Request.AddHeader(ContentTypeHeaderFull);
    Check(Self.Module.HasUnknownContentType(Request),
          'Nothing else is supported');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestRejectNonInviteWithReplacesHeader;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.AddHeader(ReplacesHeader).Value := '1;from-tag=2;to-tag=3';

    Self.MarkSentResponseCount;
    Self.ReceiveRequest(Request);
    CheckResponseSent('No response sent');
    CheckEquals(SIPBadRequest,
                Self.LastSentResponse.StatusCode,
                'Unexpected response sent');
  finally
    Request.Free;
  end;
end;

procedure TestTIdSipMessageModule.TestRejectNoSupportedMimeTypesInAccept;
var
  Request: TIdSipRequest;
begin
  Request := Self.Core.CreateRequest(MethodOptions, Self.Destination);
  try
    Request.Accept.Value := 'x-application/unknown';

    Self.MarkSentResponseCount;
    Self.ReceiveRequest(Request);
    CheckResponseSent('No response sent');
    CheckEquals(SIPNotAcceptableClient,
                Self.LastSentResponse.StatusCode,
                'Unexpected response sent');
  finally
    Request.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipNullModule                                                       *
//******************************************************************************
//* TestTIdSipNullModule Public methods ****************************************

procedure TestTIdSipNullModule.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.ModuleFor('No such method');
end;

//* TestTIdSipNullModule Published methods *************************************

procedure TestTIdSipNullModule.TestIsNull;
begin
  CheckEquals(TIdSipNullModule.ClassName,
              Self.Module.ClassName,
              'Wrong module');
  Check(Self.Module.IsNull,
        'Null message module not marked as null');
end;

//******************************************************************************
//* TestTIdSipRedirectedAction                                                 *
//******************************************************************************
//* TestTIdSipRedirectedAction Public methods **********************************

procedure TestTIdSipRedirectedAction.SetUp;
begin
  inherited SetUp;

  Self.Action := TIdSipRedirectedAction.Create(Self.Core);
end;

procedure TestTIdSipRedirectedAction.TearDown;
begin
  Self.Action.Free;

  inherited TearDown;
end;

//* TestTIdSipRedirectedAction Published methods *******************************

procedure TestTIdSipRedirectedAction.TestMethodSetMethod;
const
  NewMethod = MethodInvite;
  OldMethod = MethodRegister;
begin
  Self.Action.SetMethod(OldMethod);
  CheckEquals(OldMethod,
              Self.Action.Method,
              'Action.Method not set');

  Self.Action.SetMethod(NewMethod);
  CheckEquals(NewMethod,
              Self.Action.Method,
              'Action.Method not reset');
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
  Options := Self.Module.CreateOptions(Self.Core.Contact);
  try
    Self.ReceiveRequest(Options);
  finally
    Options.Free;
  end;
end;

//* TestTIdSipOptionsModule Published methods **********************************

procedure TestTIdSipOptionsModule.TestCreateOptions;
var
  Options: TIdSipRequest;
begin
  Options := Self.Module.CreateOptions(Self.Destination);
  try
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

  Self.Core.DoNotDisturb := false;

  Self.MarkSentResponseCount;

  Self.ReceiveOptions;
  CheckResponseSent('No response sent when UA not set to Do Not Disturb');

  CheckEquals(SIPOK,
              Self.LastSentResponse.StatusCode,
              'Wrong response sent: Do Not Disturb set off');
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
  Options := Self.Module.CreateOptions(Self.Destination);
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
//* TestTIdSipInboundOptions                                                   *
//******************************************************************************
//* TestTIdSipInboundOptions Public methods ************************************

procedure TestTIdSipInboundOptions.SetUp;
begin
  inherited SetUp;

  Self.Invite.Method := MethodOptions;
  Self.Options := TIdSipInboundOptions.CreateInbound(Self.Core,
                                                     Self.Invite,
                                                     false);
end;

procedure TestTIdSipInboundOptions.TearDown;
begin
  Self.Options.Free;

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
  Check(Self.Core.Contact.Equals(Response.FirstContact),
        'Contact header value');

  Check(Response.HasHeader(WarningHeader),
        'No Warning header');
  CheckEquals(Self.Core.Hostname,
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
                                                   false);
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
  Options := Self.Core.QueryOptions(Self.Destination);
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
  Options := Self.Core.QueryOptions(Self.Core.From);
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

    OptionsCount := Self.Core.OptionsCount;

    Self.ReceiveResponse(StatusCode * 100);

    Check(Self.ReceivedResponse,
          'Listeners not notified of response ' + IntToStr(StatusCode * 100));
    Check(Self.Core.OptionsCount < OptionsCount,
          'OPTIONS action not terminated for ' + IntToStr(StatusCode) + ' response');
  end;
end;

procedure TestTIdSipOutboundOptions.TestRemoveListener;
var
  L1, L2:  TIdSipTestOptionsListener;
  Options: TIdSipOutboundOptions;
begin
  Options := Self.Core.QueryOptions(Self.Core.From);
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

//******************************************************************************
//* TestTIdSipActionRegistry                                                   *
//******************************************************************************
//* TestTIdSipActionRegistry Public methods ************************************

procedure TestTIdSipActionRegistry.SetUp;
begin
  inherited SetUp;

  Self.Core    := TIdSipUserAgent.Create;
  Self.Request := TIdSipTestResources.CreateBasicRequest;
end;

procedure TestTIdSipActionRegistry.TearDown;
begin
  Self.Request.Free;
  Self.Core.Free;

  inherited TearDown;
end;

//* TestTIdSipActionRegistry Published methods *********************************

procedure TestTIdSipActionRegistry.TestActionsAddToRegistryAutomatically;
var
  InviteAction: TIdSipAction;
begin
  InviteAction := TIdSipOutboundInitialInvite.Create(Self.Core);
  try
    CheckNotEquals('', InviteAction.ID, 'Action has no ID');
    Check(nil <> TIdSipActionRegistry.FindAction(InviteAction.ID),
          'Action not added to registry');
  finally
    InviteAction.Free;
  end;
end;

procedure TestTIdSipActionRegistry.TestActionsGetUniqueIDs;
var
  InviteAction:  TIdSipAction;
  OptionsAction: TIdSipAction;
begin
  // This test isn't exactly thorough: it's not possible to write a test that
  // proves the registry will never duplicate an existing Action's ID,
  // but this at least demonstrates that the registry won't return the same
  // ID twice in a row.

  InviteAction := TIdSipOutboundInitialInvite.Create(Self.Core);
  try
    OptionsAction := TIdSipOutboundOptions.Create(Self.Core);
    try
      CheckNotEquals(InviteAction.ID,
                     OptionsAction.ID,
                     'The registry gave two Actions the same ID');
    finally
      OptionsAction.Free;
    end;
  finally
    InviteAction.Free;
  end;
end;

procedure TestTIdSipActionRegistry.TestActionsAutomaticallyUnregister;
var
  InviteAction: TIdSipAction;
  ActionID:     String;
begin
  InviteAction := TIdSipOutboundInitialInvite.Create(Self.Core);
  try
    ActionID := InviteAction.ID;
  finally
    InviteAction.Free;
  end;

  Check(nil = TIdSipActionRegistry.FindAction(ActionID),
        'Action not removed from registry');
end;

//******************************************************************************
//* TestTIdSipActionSendWait                                                   *
//******************************************************************************
//* TestTIdSipActionSendWait Public methods ************************************

procedure TestTIdSipActionSendWait.SetUp;
begin
  inherited SetUp;

  Self.Action := Self.Core.RegisterModule.RegisterWith(Self.Destination.Address);
  Self.Wait := TIdSipActionSendWait.Create;
  Self.Wait.ActionID := Self.Action.ID;
end;

procedure TestTIdSipActionSendWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipActionSendWait Published methods *********************************

procedure TestTIdSipActionSendWait.TestTrigger;
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('No request sent, so Wait didn''t Trigger');
end;

procedure TestTIdSipActionSendWait.TestTriggerOnNonExistentAction;
begin
  Self.Wait.ActionID := '';

  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckNoRequestSent('A request sent, so something mapped the Wait to an '
                   + 'unexpected Action');
end;

//******************************************************************************
//* TestTIdSipActionTerminateWait                                              *
//******************************************************************************
//* TestTIdSipActionTerminateWait Public methods *******************************

procedure TestTIdSipActionTerminateWait.SetUp;
begin
  inherited SetUp;

  Self.Action := Self.Core.InviteModule.Call(Self.Destination, '', '');
  Self.Wait   := TIdSipActionTerminateWait.Create;
  Self.Wait.ActionID := Self.Action.ID;
end;

procedure TestTIdSipActionTerminateWait.TearDown;
begin
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipActionTerminateWait Published methods ****************************

procedure TestTIdSipActionTerminateWait.TestTrigger;
begin
  // When we Trigger the Wait, the Action, having received at least one response
  // from the remote party, will send a CANCEL.

  Self.Action.Send;
  Self.ReceiveRinging(Self.LastSentRequest);

  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('No request sent, so Action didn''t Trigger');
  CheckEquals(MethodCancel,
              Self.LastSentRequest.Method,
              'Unexpected message sent');
end;

procedure TestTIdSipActionTerminateWait.TestTriggerOnNonExistentAction;
begin
  Self.Wait.ActionID := '';

  // This test just makes sure the Trigger doesn't blow up when called on a
  // non-existent action ID.
  Self.Wait.Trigger;
end;

//******************************************************************************
//* TestTIdSipActionAuthenticationChallengeMethod                              *
//******************************************************************************
//* TestTIdSipActionAuthenticationChallengeMethod Public methods ***************

procedure TestTIdSipActionAuthenticationChallengeMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Action := Self.UA.QueryOptions(Nowhere);
  finally
    Nowhere.Free;
  end;

  Self.Listener := TIdSipMockListener.Create;

  Self.Method := TIdSipActionAuthenticationChallengeMethod.Create;
  Self.Method.ActionAgent := Self.Action;
  Self.Method.Challenge   := Self.Response;
end;

procedure TestTIdSipActionAuthenticationChallengeMethod.TearDown;
begin
  Self.Method.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//* TestTIdSipActionAuthenticationChallengeMethod Published methods ************

procedure TestTIdSipActionAuthenticationChallengeMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.AuthenticationChallenged,
        'Listener not notified');
  Check(Self.Action = Self.Listener.ActionParam,
        'Action param');
  Check(Self.Response = Self.Listener.ResponseParam,
        'Response param');
end;

//******************************************************************************
//* TestTIdSipActionNetworkFailureMethod                                       *
//******************************************************************************
//* TestTIdSipActionNetworkFailureMethod Public methods ************************

procedure TestTIdSipActionNetworkFailureMethod.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Action := Self.UA.QueryOptions(Nowhere);
  finally
    Nowhere.Free;
  end;

  Self.Listener := TIdSipMockListener.Create;
  Self.Method   := TIdSipActionNetworkFailureMethod.Create;

  Self.ErrorCode := 13;
  Self.Reason    := 'The most random number';

  Self.Method.ActionAgent := Self.Action;
  Self.Method.ErrorCode   := Self.ErrorCode;
  Self.Method.Reason      := Self.Reason;
end;

procedure TestTIdSipActionNetworkFailureMethod.TearDown;
begin
  Self.Method.Free;
  Self.Listener.Free;

  inherited TearDown;
end;

//* TestTIdSipActionNetworkFailureMethod Published methods *********************

procedure TestTIdSipActionNetworkFailureMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.NetworkFailed,
        'Listener not notified');
  Check(Self.Action = Self.Listener.ActionParam,
        'Action param');
  CheckEquals(Self.ErrorCode,
              Self.Listener.ErrorCodeParam,
              'Error code');
  CheckEquals(Self.Reason,
              Self.Listener.ReasonParam,
            'Reason');
end;

//******************************************************************************
//* TOwnedActionMethodTestCase                                                 *
//******************************************************************************
//* TOwnedActionMethodTestCase Public methods **********************************

procedure TOwnedActionMethodTestCase.SetUp;
var
  Nowhere: TIdSipAddressHeader;
begin
  inherited SetUp;

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Action := TIdSipOutboundInvite.Create(Self.UA);
  finally
    Nowhere.Free;
  end;

  Self.Listener := TIdSipOwnedActionListener.Create;
end;

procedure TOwnedActionMethodTestCase.TearDown;
begin
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipOwnedActionFailureMethod                                         *
//******************************************************************************
//* TestTIdSipOwnedActionFailureMethod Public methods **************************

procedure TestTIdSipOwnedActionFailureMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipOwnedActionFailureMethod.Create;

  Self.Method.ActionAgent := Self.Action;
  Self.Method.Reason      := 'none';
  Self.Method.Response    := Self.Response;
end;

procedure TestTIdSipOwnedActionFailureMethod.TearDown;
begin
  Self.Method.Free;
  Self.Action.Free; // We created the action, so the UA doesn't own it.

  inherited TearDown;
end;

//* TestTIdSipOwnedActionFailureMethod Published methods ***********************

procedure TestTIdSipOwnedActionFailureMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Failure, 'Listener not notified');
  Check(Self.Method.ActionAgent = Self.Listener.ActionParam,
        'InviteAgent param');
  Check(Self.Method.Response = Self.Listener.ResponseParam,
        'Response param');
  CheckEquals(Self.Method.Reason,
              Self.Listener.ReasonParam,
              'Reason param');
end;

//******************************************************************************
//* TestTIdSipOwnedActionRedirectMethod                                        *
//******************************************************************************
//* TestTIdSipOwnedActionRedirectMethod Public methods *************************

procedure TestTIdSipOwnedActionRedirectMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipOwnedActionRedirectMethod.Create;

  Self.Method.ActionAgent := Self.Action;
  Self.Method.Response    := Self.Response;
end;

procedure TestTIdSipOwnedActionRedirectMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOwnedActionRedirectMethod Published methods **********************

procedure TestTIdSipOwnedActionRedirectMethod.Run;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Redirected, 'Listener not notified');
  Check(Self.Method.ActionAgent = Self.Listener.ActionParam,
        'Action param');
  Check(Self.Method.Response = Self.Listener.RedirectParam,
        'Response param');
end;

//******************************************************************************
//* TestTIdSipOwnedActionSuccessMethod                                         *
//******************************************************************************
//* TestTIdSipOwnedActionSuccessMethod Public methods **************************

procedure TestTIdSipOwnedActionSuccessMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipOwnedActionSuccessMethod.Create;

  Self.Method.ActionAgent := Self.Action;
  Self.Method.Msg         := Self.Response;
end;

procedure TestTIdSipOwnedActionSuccessMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipOwnedActionSuccessMethod Published methods ***********************

procedure TestTIdSipOwnedActionSuccessMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Success, 'Listener not notified');
  Check(Self.Method.ActionAgent = Self.Listener.ActionParam,
        'Action param');
  Check(Self.Method.Msg = Self.Listener.MsgParam,
        'Response param');
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

  Nowhere := TIdSipAddressHeader.Create;
  try
    Self.Method.Options  := Self.UA.QueryOptions(Nowhere);
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

//******************************************************************************
//* TestTIdSipActionRedirectorMethod                                           *
//******************************************************************************
//* TestTIdSipActionRedirectorMethod Public methods ****************************

procedure TestTIdSipActionRedirectorMethod.SetUp;
var
  ArbitraryAction: TIdSipOutboundSession;
begin
  inherited SetUp;

  Self.Listener   := TIdSipMockActionRedirectorListener.Create;
  ArbitraryAction := Self.UA.InviteModule.Call(Self.UA.Contact, '', '');
  Self.Redirector := TIdSipActionRedirector.Create(ArbitraryAction);
end;

procedure TestTIdSipActionRedirectorMethod.TearDown;
begin
  Self.Redirector.Free;
  
  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipRedirectorRedirectFailureMethod                                  *
//******************************************************************************
//* TestTIdSipRedirectorRedirectFailureMethod Public methods *******************

procedure TestTIdSipRedirectorRedirectFailureMethod.SetUp;
begin
  inherited SetUp;

  Self.ErrorCode  := RedirectWithNoContacts;
  Self.Reason     := RSRedirectWithNoContacts;

  Self.Method := TIdSipRedirectorRedirectFailureMethod.Create;
  Self.Method.ErrorCode  := Self.ErrorCode;
  Self.Method.Reason     := Self.Reason;
  Self.Method.Redirector := Self.Redirector;
end;

procedure TestTIdSipRedirectorRedirectFailureMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRedirectorRedirectFailureMethod Published methods ****************

procedure TestTIdSipRedirectorRedirectFailureMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.RedirectFailed, 'Listener not notified');
  Check(Self.Method.Redirector = Self.Listener.RedirectorParam,
        'Redirector param');
   CheckEquals(Self.Method.ErrorCode,
               Self.Listener.ErrorCodeParam,
               'ErrorCode param');
   CheckEquals(Self.Method.Reason,
               Self.Listener.ReasonParam,
               'Reason param');
end;

//******************************************************************************
//* TestTIdSipRedirectorNewActionMethod                                        *
//******************************************************************************
//* TestTIdSipRedirectorNewActionMethod Public methods *************************

procedure TestTIdSipRedirectorNewActionMethod.SetUp;
begin
  inherited SetUp;

  Self.NewAction := Self.UA.QueryOptions(Self.UA.Contact);

  Self.Method := TIdSipRedirectorNewActionMethod.Create;
  Self.Method.NewAction  := Self.NewAction;
  Self.Method.Redirector := Self.Redirector;
end;

procedure TestTIdSipRedirectorNewActionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRedirectorNewActionMethod Published methods **********************

procedure TestTIdSipRedirectorNewActionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.NewAction, 'Listener not notified');
  Check(Self.Method.Redirector = Self.Listener.RedirectorParam,
        'Redirector param');
  Check(Self.Method.NewAction = Self.Listener.NewActionParam,
        'NewAction param');
end;

//******************************************************************************
//* TestTIdSipRedirectorSuccessMethod                                          *
//******************************************************************************
//* TestTIdSipRedirectorSuccessMethod Public methods ***************************

procedure TestTIdSipRedirectorSuccessMethod.SetUp;
begin
  inherited SetUp;

  Self.Response := TIdSipResponse.Create;
  Self.Method   := TIdSipRedirectorSuccessMethod.Create;

  Self.Method.Redirector := Self.Redirector;
  Self.Method.Response   := Self.Response;
end;

procedure TestTIdSipRedirectorSuccessMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;

  inherited TearDown;
end;

//* TestTIdSipRedirectorSuccessMethod Published methods ************************

procedure TestTIdSipRedirectorSuccessMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Succeeded, 'Listener not notified');
  Check(Self.Method.Redirector = Self.Listener.RedirectorParam,
        'Redirector param');
  Check(Self.Method.Response = Self.Listener.ResponseParam,
        'Response param');
  Check(Self.Method.SuccessfulAction = Self.Listener.SuccessfulActionParam,
        'SuccessfulAction param');
end;

//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod                           *
//******************************************************************************
//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Public methods ************

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.SetUp;
begin
  inherited SetUp;

  Self.Receiver := TIdSipMockUdpTransport.Create;
  Self.Response := TIdSipResponse.Create;

  Self.Method := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  Self.Method.Receiver := Self.Receiver;
  Self.Method.Message := Self.Response.Copy;
end;

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TearDown;
begin
  Self.Method.Free;
  Self.Response.Free;
  Self.Receiver.Free;

  inherited TearDown;
end;

//* TestTIdSipUserAgentDroppedUnmatchedMessageMethod Published methods *********

procedure TestTIdSipUserAgentDroppedUnmatchedMessageMethod.TestRun;
var
  L: TIdSipTestTransactionUserListener;
begin
  L := TIdSipTestTransactionUserListener.Create;
  try
    Self.Method.Run(L);

    Check(L.DroppedUnmatchedMessage, 'Listener not notified');
    Check(Self.Method.Receiver = L.ReceiverParam,
          'Receiver param');
    Check(Self.Method.Message = L.MessageParam,
          'Message param');
    Check(Self.Method.UserAgent = L.AbstractUserAgentParam,
          'UserAgent param');
  finally
    L.Free;
  end;
end;

initialization
  RegisterTest('Transaction User Cores', Suite);
end.

