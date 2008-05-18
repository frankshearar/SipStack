{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipHeaders;

interface

uses
  IdSipMessage, TestFramework, TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestDecodeQuotedStr;
    procedure TestIsQuoted;
    procedure TestNeedsQuotes;
    procedure TestQuoteStringIfNecessary;
    procedure TestQValueToStr;
    procedure TestReadDigit;
    procedure TestStrToQValue;
    procedure TestStrToQValueDef;
  end;

  TIdSipParameterTestCase = class(TTestCase)
  protected
    P: TIdSipParameter;

    function CreateParameter: TIdSipParameter; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsStringParameterHasNoValue;
    procedure TestEquals;
    procedure TestName;
    procedure TestValue; virtual;
  end;

  TestTIdSipParameter = class(TIdSipParameterTestCase)
  protected
    function CreateParameter: TIdSipParameter; override;
  published
    procedure TestAsHeaderParameter;
    procedure TestAsString;
    procedure TestAsStringWithEscapedCharacters;
    procedure TestAsUriParameter;
    procedure TestValue; override;
  end;

  TestTIdSipQuotedStringParameter = class(TIdSipParameterTestCase)
  protected
    function CreateParameter: TIdSipParameter; override;
  published
    procedure TestAsHeaderParameter;
    procedure TestAsString;
    procedure TestValue; override;
  end;

  TIdSipParametersTestCase = class(TTestCase)
  private
    Params: TIdSipParameters;
  protected
    function CreateParameters: TIdSipParameters; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddParamTypeDependsOnNameParameter;
    procedure TestAddParamAndHasParam;
    procedure TestAdd;
    procedure TestAssign;
    procedure TestAssignToNonEmptyList;
    procedure TestAsString; virtual;
    procedure TestClear;
    procedure TestCount;
    procedure TestDoubleAdd;
    procedure TestEquals;
    procedure TestHasParam;
    procedure TestParameterTypes;
    procedure TestParamValue;
    procedure TestRemoveParameter;
    procedure TestRemoveParameterParameterMissing;
    procedure TestIntersectionEquals;
  end;

  TestTIdSipHeaderParameters = class(TIdSipParametersTestCase)
  protected
    function CreateParameters: TIdSipParameters; override;
  published
    procedure TestAsString; override;
    procedure TestIsMalformed;
    procedure TestParse;
    procedure TestParseMalformedQuotedString;
    procedure TestParseMissingValue;
    procedure TestParseMultipleParameters;
    procedure TestParseNoNameNoValue;
    procedure TestParseQuotedString;
    procedure TestParseQuotedStringContainsSemicolon;
    procedure TestParseSimple;
    procedure TestParseTorture;
    procedure TestParseValuelessParameter;
    procedure TestParseWithWhitespace;
  end;

  TestTIdSipUriParameters = class(TIdSipParametersTestCase)
  protected
    function CreateParameters: TIdSipParameters; override;
  published
     procedure TestAsString; override;
     procedure TestIsMalformed;
  end;

  THeaderTestCase = class(TTestCaseSip)
  protected
    Header: TIdSipHeader;
    function HeaderType: TIdSipHeaderClass; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsContact; virtual;
    procedure TestValue; virtual;
  end;

  TestTIdSipHeader = class(THeaderTestCase)
  private
    H: TIdSipHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAsString;
    procedure TestAssignCopiesParseInfo;
    procedure TestCanonicaliseName;
    procedure TestFullValue;
    procedure TestGetHeaderName;
    procedure TestGetHeaderValue;
    procedure TestGetSetParam;
    procedure TestHasParam;
    procedure TestEquals;
    procedure TestParamCount;
    procedure TestParamsAsString;
    procedure TestRemoveAbsentParameter;
    procedure TestRemoveParameter;
    procedure TestValueParameterClearing;
    procedure TestValueWithNewParams;
    procedure TestValueWithQuotedParams;
    procedure TestUnparsedValue;
  end;

  TestTIdSipAddressHeader = class(THeaderTestCase)
  private
    A: TIdSipAddressHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAsAddressOfRecord;
    procedure TestAsString;
    procedure TestAsCanonicalAddress;
    procedure TestAsToHeader;
    procedure TestGrid;
    procedure TestHasSipsUri;
    procedure TestIsMalformed;
    procedure TestIsMalformedAfterSettingProperties;
    procedure TestIsMalformedWithParsingAndManipulation;
    procedure TestSetAddress;
    procedure TestValue; override;
    procedure TestValueEmptyDisplayName;
    procedure TestValueEmptyString;
    procedure TestValueMissingScheme;
    procedure TestValueWithBlankQuotedName;
    procedure TestValueWithEncodings;
    procedure TestValueWithMalformedQuotedName;
    procedure TestValueWithNormalName;
    procedure TestValueWithNoWhitespaceBetweenDisplayNameAndUri;
    procedure TestValueWithParam;
    procedure TestValueWithQuotedName;
    procedure TestValueWithSpaceInDisplayName;
    procedure TestValueWithSpecialChars;
    procedure TestValueWithTrailingWhitespacePlusParam;
    procedure TestValueWithUnquotedNonTokensPlusParam;
    procedure TestValueWithQuotedURN;
  end;

  TestTIdSipCommaSeparatedHeader = class(THeaderTestCase)
  private
    C: TIdSipCommaSeparatedHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestRemoveValues;
    procedure TestValue; override;
  end;

  TestTIdSipAllowEventsHeader = class(TestTIdSipCommaSeparatedHeader)
  private
    AE: TIdSipAllowEventsHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestIsMalformed;
    procedure TestValue; override;
  end;

  TestTIdSipAuthorizationHeader = class(THeaderTestCase)
  private
    A: TIdSipAuthorizationHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAlgorithm;
    procedure TestCNonce;
    procedure TestDigestResponse;
    procedure TestDigestUri;
    procedure TestGetValue;
    procedure TestHasParam;
    procedure TestIsBasic;
    procedure TestIsDigest;
    procedure TestName; virtual;
    procedure TestNC;
    procedure TestNonce;
    procedure TestIsNonce;
    procedure TestNonceCount;
    procedure TestOpaque;
    procedure TestQop;
    procedure TestRealm;
    procedure TestUnknownResponses;
    procedure TestUnquotedResponse;
    procedure TestUsername;
    procedure TestValue; override;
    procedure TestValueQuotedNonceCount;
    procedure TestValueQuotedQop;
    procedure TestValueSingleParam;
  end;

  TestTIdSipCallIDHeader = class(THeaderTestCase)
  private
    C: TIdSipCallIDHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestEquals;
    procedure TestValue; override;
    procedure TestValueWithParams;
  end;

  TestTIdSipContactHeader = class(THeaderTestCase)
  private
    C: TIdSipContactHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAssign;
    procedure TestIsContact; override;
    procedure TestIsMalformedWildCardUri;
    procedure TestName;
    procedure TestGetSetExpires;
    procedure TestGetSetQ;
    procedure TestGetValueWithStar;
    procedure TestGruu;
    procedure TestRemoveExpires;
    procedure TestValue; override;
    procedure TestValueWithExpires;
    procedure TestValueInstanceID;
    procedure TestValueWithQ;
    procedure TestValueWithParams;
    procedure TestValueWithStar;
    procedure TestWillExpire;
  end;

  TestTIdSipContentDispositionHeader = class(THeaderTestCase)
  private
    C: TIdSipContentDispositionHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetSetHandling;
    procedure TestIsSession;
    procedure TestName;
    procedure TestValue; override;
  end;

  TestTIdSipCSeqHeader = class(THeaderTestCase)
  private
    C: TIdSipCSeqHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestIncrement;
    procedure TestValue; override;
    procedure TestVeryLargeLegalValue;
    procedure TestVeryLargeValue;
  end;

  TestTIdSipDateHeader = class(THeaderTestCase)
  private
    D: TIdSipDateHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestName;
    procedure TestGetValue;
    procedure TestValue; override;
    procedure TestValueMalformedAbsoluteTime;
    procedure TestValueRelativeTime;
    procedure TestValueZeroTime;
  end;

  TestTIdSipEventHeader = class(THeaderTestCase)
  private
    E: TIdSipEventHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestEquals;
    procedure TestEqualsWithID;
    procedure TestEqualsWithNonIDParams;
    procedure TestGetSetID;
    procedure TestIsEventType;
    procedure TestIsTokenNoDot;
    procedure TestMalformedValue;
    procedure TestValue; override;
    procedure TestValueWithID;
  end;

  TestTIdSipFromToHeader = class(THeaderTestCase)
  private
    F: TIdSipFromToHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestCopyWithoutTag;
    procedure TestHasTag;
    procedure TestIsEqualDifferentURI;
    procedure TestIsEqualSameURINoParams;
    procedure TestIsEqualSameURIWithParams;
    procedure TestIsMalformedWithBlankTag;
    procedure TestValue; override;
    procedure TestValueWithTag;
    procedure TestValueResettingTag;
    procedure TestGetSetTag;
  end;

  TestTIdSipMaxForwardsHeader = class(THeaderTestCase)
  private
    M: TIdSipMaxForwardsHeader;
  protected
   function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestName;
    procedure TestValue; override;
    procedure TestValueNonNumber;
    procedure TestValueTooBig;
    procedure TestValueWithParam;
  end;

  TestTIdSipNumericHeader = class(THeaderTestCase)
  private
    N: TIdSipNumericHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestValue; override;
    procedure TestValueWithMultipleTokens;
    procedure TestValueWithNegativeNumber;
    procedure TestValueWithString;
    procedure TestVeryLargeValue;
    procedure TestVeryLargeLegalValue;
  end;

  TestTIdSipAuthenticateHeader = class(THeaderTestCase)
  private
     A: TIdSipAuthenticateHeader;
  public
    procedure SetUp; override;
  published
    procedure TestDomain;
    procedure TestName; virtual;
    procedure TestRemoveStaleResponse;
    procedure TestStale;
    procedure TestValue; override;
  end;

  TestTIdSipProxyAuthenticateHeader = class(TestTIdSipAuthenticateHeader)
  private
    P: TIdSipProxyAuthenticateHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestCredentialHeaderType;
    procedure TestName; override;
  end;

  TestTIdSipAuthenticationInfoHeader = class(THeaderTestCase)
  private
    A: TIdSipAuthenticationInfoHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestCNonce;
    procedure TestHasNextNonce;
    procedure TestName;
    procedure TestNextNonce;
    procedure TestResponseDigest;
    procedure TestValue; override;
  end;

  TestTIdSipProxyAuthorizationHeader = class(TestTIdSipAuthorizationHeader)
  private
    P: TIdSipProxyAuthorizationHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestName; override;
  end;

  TestTIdSipRetryAfterHeader = class(THeaderTestCase)
  private
    R: TIdSipRetryAfterHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetValue;
    procedure TestHasDuration;
    procedure TestMalformedValue;
    procedure TestName;
    procedure TestValue; override;
    procedure TestValueWithComment;
    procedure TestValueWithCommentAndDuration;
    procedure TestValueWithDuration;
    procedure TestValueWithOddComments;
    procedure TestValueWithOddWhitespace;
    procedure TestValueWithUtf8Comment;
  end;

  TRouteHeaderTestCase = class(THeaderTestCase)
  protected
    R: TIdSipRouteHeader;
  public
    procedure SetUp; override;
  published
    procedure TestIsLooseRoutable;
    procedure TestValue; override;
    procedure TestValueWithLeadingSpaceParam;
    procedure TestValueWithParamsAndHeaderParams;
  end;

  TestTIdSipRouteHeader = class(TRouteHeaderTestCase)
  protected
    function HeaderType: TIdSipHeaderClass; override;
  published
    procedure TestIsLooseRoutable;
    procedure TestName;
  end;

  TestTIdSipRecordRouteHeader = class(TRouteHeaderTestCase)
  protected
    function HeaderType: TIdSipHeaderClass; override;
  published
    procedure TestName;
  end;

  TestTIdSipReferToHeader = class(TestTIdSipAddressHeader)
  private
    R: TIdSipReferToHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  end;

  TestTIdSipParameteredCallIDHeader = class(THeaderTestCase)
  private
    C: TIdSipParameteredCallIDHeader;
  public
    procedure SetUp; override;
  published
    procedure TestCallID;
    procedure TestSetCallID;
    procedure TestValue; override;
  end;

  TestTIdSipReplacesHeader = class(TestTIdSipParameteredCallIDHeader)
  private
    R: TIdSipReplacesHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestFromTag;
    procedure TestIsEarly;
    procedure TestName;
    procedure TestToTag;
    procedure TestValue; override;
    procedure TestValueMissingFromTag;
    procedure TestValueMissingToTag;
    procedure TestValueMultipleFromTags;
    procedure TestValueMultipleToTags;
  end;

  TestTIdSipSubscriptionStateHeader = class(THeaderTestCase)
  private
    SS: TIdSipSubscriptionStateHeader;

  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAssign;
    procedure TestGetReasonType;
    procedure TestHasGivenUp;
    procedure TestHasRetryAfter;
    procedure TestIsActive;
    procedure TestIsDeactivated;
    procedure TestIsInProbation;
    procedure TestIsNoResource;
    procedure TestIsPending;
    procedure TestIsRejected;
    procedure TestIsTerminated;
    procedure TestIsTimedOut;
    procedure TestReasonTypeToStr;
    procedure TestRetryAfterHasMeaning;
    procedure TestSetReasonType;
    procedure TestStrToReasonType;
    procedure TestValue; override;
  end;

  TestTIdSipTargetDialogHeader = class(TestTIdSipParameteredCallIDHeader)
  private
    T: TIdSipTargetDialogHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAssignDoesntNormallyRaiseAnException;
    procedure TestHasCompleteDialogID;
    procedure TestLocalTag;
    procedure TestName;
    procedure TestRemoteTag;
    procedure TestValue; override;
    procedure TestValueMissingLocalTag;
    procedure TestValueMissingRemoteTag;
    procedure TestValueMultipleLocalTags;
    procedure TestValueMultipleRemoteTags;
  end;

  TestTIdSipTimestampHeader = class(THeaderTestCase)
  private
    T: TIdSipTimestampHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestName;
    procedure TestNormalizeLWS;
    procedure TestReadNumber;
    procedure TestValue; override;
    procedure TestValueMalformed;
    procedure TestValueWithDelay;
  end;

  TestTIdSipUriHeader = class(THeaderTestCase)
  private
    U: TIdSipUriHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestEqualsDifferentHeader;
    procedure TestEqualsDifferentURI;
    procedure TestEqualsSameHeaderValueDifferentClass;
    procedure TestEqualsSameURINoParams;
    procedure TestEqualsSameURIWithParams;
    procedure TestIsGruu;
    procedure TestValue; override;
    procedure TestValueWithParams;
    procedure TestValueWithUriParams;
  end;

  TestTIdSipViaHeader = class(THeaderTestCase)
  private
    V: TIdSipViaHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignWithDefaultPortSpecified;
    procedure TestAsUri;
    procedure TestBranch;
    procedure TestHasBranch;
    procedure TestHasMaddr;
    procedure TestHasReceived;
    procedure TestHasRport;
    procedure TestIsDefaultPortForTransport;
    procedure TestIsRFC3261Branch;
    procedure TestEquals;
    procedure TestEqualsBranchIsCaseInsensitive;
    procedure TestMaddr;
    procedure TestName;
    procedure TestReceived;
    procedure TestRemoveBranch;
    procedure TestRoutingAddress;
    procedure TestRoutingPort;
    procedure TestSrvQuery;
    procedure TestTTL;
    procedure TestValue; override;
    procedure TestValueTorture;
    procedure TestValueWithBranch;
    procedure TestValueWithDefaultPort;
    procedure TestValueWithIPv6NumericAddress;
    procedure TestValueWithIPv6NumericAddressAndPort;
    procedure TestValueWithSettingDefaultPort;
    procedure TestValueWithMaddr;
    procedure TestValueWithReceived;
    procedure TestValueWithTTL;
  end;

  TestTIdSipWarningHeader = class(THeaderTestCase)
  private
    W: TIdSipWarningHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestGetValue;
    procedure TestName;
    procedure TestValue; override;
    procedure TestSetValueIPv6;
    procedure TestSetValueMalformed;
    procedure TestSetValuePortSpecified;
    procedure TestSetValueToken;
  end;

  TestTIdSipWeightedCommaSeparatedHeader = class(THeaderTestCase)
  private
    W: TIdSipWeightedCommaSeparatedHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestAddValue;
    procedure TestClearValues;
    procedure TestGetValue;
    procedure TestValue; override;
    procedure TestValueMalformed;
  end;

  TestTIdSipWWWAuthenticateHeader = class(TestTIdSipAuthenticateHeader)
  private
     W: TIdSipWWWAuthenticateHeader;
  protected
    function HeaderType: TIdSipHeaderClass; override;
  public
    procedure SetUp; override;
  published
    procedure TestCredentialHeaderType;
    procedure TestName; override;
  end;

  TTestHeadersList = class(TTestCase)
  protected
    Headers: TIdSipHeaders;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddInReverseOrder; virtual;
  end;

  TestTIdSipHeadersFilter = class(TTestHeadersList)
  private
    Filter:  TIdSipHeadersFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddInReverseOrder; override;
    procedure TestCount;
    procedure TestFirst;
    procedure TestIsEmpty;
    procedure TestEqualsFilter;
    procedure TestEqualsHeaders;
    procedure TestEqualsOrderIrrelevant;
    procedure TestItems;
    procedure TestIteratorVisitsAllHeaders;
    procedure TestRemove;
    procedure TestRemoveAll;
  end;

  TestTIdSipHeaders = class(TTestHeadersList)
  private
    procedure CheckType(ExpectedClassType: TClass;
                        ReceivedObject: TObject;
                        Message: String = '');
  published
    procedure TestAddAndCount;
    procedure TestAddHeader;
    procedure TestAddHeaderName;
    procedure TestAddHeaders;
    procedure TestAddHeadersEmptyList;
    procedure TestAddHeadersFilter;
    procedure TestAddInReverseOrder; override;
    procedure TestAddResultTypes;
    procedure TestAsString;
    procedure TestClear;
    procedure TestDelete;
    procedure TestEqualsPenultimateHeaderNotEqual;
    procedure TestFirst;
    procedure TestGetAllButFirst;
    procedure TestHasHeader;
    procedure TestIsMalformed;
    procedure TestHeaders;
    procedure TestItems;
    procedure TestIsCallID;
    procedure TestIsCompoundHeader;
    procedure TestIsContact;
    procedure TestIsContentLength;
    procedure TestIsCSeq;
    procedure TestIsEmpty;
    procedure TestIsErrorInfo;
    procedure TestIsFrom;
    procedure TestIsMaxForwards;
    procedure TestIsRecordRoute;
    procedure TestIsRoute;
    procedure TestIsTo;
    procedure TestIsVia;
    procedure TestIsWarning;
    procedure TestIteratorVisitsAllHeaders;
    procedure TestRemove;
    procedure TestRemoveAll;
  end;

  TestTIdSipAuthorizations = class(TTestCase)
  private
    Headers:        TIdSipHeaders;
    Authorizations: TIdSipAuthorizations;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateOnEmptySet;
    procedure TestCurrentAuthorization;
  end;

  TestTIdSipContacts = class(TTestCase)
  private
    Headers:  TIdSipHeaders;
    Contacts: TIdSipContacts;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestContactFor;
    procedure TestCreateOnEmptySet;
    procedure TestCurrentContact;
    procedure TestGruuFor;
    procedure TestHasContact;
    procedure TestRemoveContact;
    procedure TestRemoveContactWithDuplicates;
  end;

  TestTIdSipExpiresHeaders = class(TTestCase)
  private
    Headers:        TIdSipHeaders;
    ExpiresHeaders: TIdSipExpiresHeaders;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCurrentExpires;
  end;

  TestTIdSipProxyAuthorizations = class(TTestCase)
  private
    Headers:        TIdSipHeaders;
    ProxyAuthorizations: TIdSipProxyAuthorizations;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateOnEmptySet;
    procedure TestCurrentProxyAuthorization;
  end;

  TestTIdSipRoutePath = class(TTestCase)
  private
    Headers: TIdSipHeaders;
    Routes:  TIdSipRoutePath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddRoute;
    procedure TestCreateOnEmptySet;
    procedure TestCurrentRoute;
    procedure TestGetAllButFirst;
  end;

  TestTIdSipViaPath = class(TTestCaseSip)
  private
    Headers: TIdSipHeaders;
    Path:    TIdSipViaPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndLastHop;
    procedure TestClear;
    procedure TestCurrentHop;
    procedure TestRemoveLastHop;
  end;

implementation

uses
  Classes, IdSipMockTransport, IdSipTransport, IdSystem, IdUnicode,
  RuntimeSafety, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipMessage tests (Headers)');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipParameter.Suite);
  Result.AddTest(TestTIdSipQuotedStringParameter.Suite);
  Result.AddTest(TestTIdSipHeaderParameters.Suite);
  Result.AddTest(TestTIdSipUriParameters.Suite);
  Result.AddTest(TestTIdSipHeader.Suite);
  Result.AddTest(TestTIdSipAddressHeader.Suite);
  Result.AddTest(TestTIdSipCommaSeparatedHeader.Suite);
  Result.AddTest(TestTIdSipAllowEventsHeader.Suite);
  Result.AddTest(TestTIdSipAuthorizationHeader.Suite);
  Result.AddTest(TestTIdSipCallIDHeader.Suite);
  Result.AddTest(TestTIdSipContactHeader.Suite);
  Result.AddTest(TestTIdSipContentDispositionHeader.Suite);
  Result.AddTest(TestTIdSipCSeqHeader.Suite);
  Result.AddTest(TestTIdSipDateHeader.Suite);
  Result.AddTest(TestTIdSipEventHeader.Suite);
  Result.AddTest(TestTIdSipFromToHeader.Suite);
  Result.AddTest(TestTIdSipMaxForwardsHeader.Suite);
  Result.AddTest(TestTIdSipNumericHeader.Suite);
  Result.AddTest(TestTIdSipProxyAuthenticateHeader.Suite);
  Result.AddTest(TestTIdSipAuthenticationInfoHeader.Suite);
  Result.AddTest(TestTIdSipProxyAuthorizationHeader.Suite);
  Result.AddTest(TestTIdSipRetryAfterHeader.Suite);
  Result.AddTest(TestTIdSipRouteHeader.Suite);
  Result.AddTest(TestTIdSipRecordRouteHeader.Suite);
  Result.AddTest(TestTIdSipReferToHeader.Suite);
  Result.AddTest(TestTIdSipReplacesHeader.Suite);
  Result.AddTest(TestTIdSipSubscriptionStateHeader.Suite);
  Result.AddTest(TestTIdSipTargetDialogHeader.Suite);
  Result.AddTest(TestTIdSipTimestampHeader.Suite);
  Result.AddTest(TestTIdSipUriHeader.Suite);
  Result.AddTest(TestTIdSipViaHeader.Suite);
  Result.AddTest(TestTIdSipWarningHeader.Suite);
  Result.AddTest(TestTIdSipWeightedCommaSeparatedHeader.Suite);
  Result.AddTest(TestTIdSipWWWAuthenticateHeader.Suite);
  Result.AddTest(TestTIdSipHeadersFilter.Suite);
  Result.AddTest(TestTIdSipHeaders.Suite);
  Result.AddTest(TestTIdSipContacts.Suite);
  Result.AddTest(TestTIdSipExpiresHeaders.Suite);
  Result.AddTest(TestTIdSipRoutePath.Suite);
  Result.AddTest(TestTIdSipViaPath.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestDecodeQuotedStr;
var
  Answer: String;
begin
  Check(DecodeQuotedStr('', Answer), 'Decoding ''''');
  CheckEquals('', Answer, 'Result of decoding ''''');

  Check(DecodeQuotedStr('\\', Answer), 'Decoding \\');
  CheckEquals('\', Answer, 'Result of decoding \\');

  Check(DecodeQuotedStr('\"', Answer), 'Decoding \"');
  CheckEquals('"', Answer, 'Result of decoding \"');

  Check(DecodeQuotedStr('\a', Answer), 'Decoding \a');
  CheckEquals('a', Answer, 'Result of decoding \a');

  Check(DecodeQuotedStr('\"foo\\bar\"', Answer), 'Decoding \"foo\\bar\"');
  CheckEquals('"foo\bar"', Answer, 'Result of decoding \"foo\\bar\"');

  Check(not DecodeQuotedStr('\', Answer), 'Decoding \');
  Check(not DecodeQuotedStr('"', Answer), 'Decoding "');
end;

procedure TestFunctions.TestIsQuoted;
begin
  Check(    IsQuoted('"I am Quoted"'),    '"I am Quoted"');
  Check(not IsQuoted('"I am not Quoted'), '"I am not Quoted');
  Check(not IsQuoted('I am not Quoted"'), 'I am not Quoted"');
  Check(not IsQuoted('I am not Quoted'),  'I am not Quoted');
  Check(not IsQuoted(''),                 '''''');
end;

procedure TestFunctions.TestNeedsQuotes;
begin
  Check(    NeedsQuotes(' '),          'SP');
  Check(    NeedsQuotes('"'),          '"');
  Check(    NeedsQuotes('\'),          '\');
  Check(    NeedsQuotes('"hello\"'),   '"hello\"');
  Check(not NeedsQuotes(''),           '''''');
  Check(    NeedsQuotes('hail eris!'), 'hail eris!');
end;

procedure TestFunctions.TestQuoteStringIfNecessary;
var
  C: Char;
begin
  CheckEquals('',     QuoteStringIfNecessary(''),     '''''');
  CheckEquals('" "',  QuoteStringIfNecessary(' '),    'SP');
  CheckEquals('abcd', QuoteStringIfNecessary('abcd'), 'abcd');

  for C := '!' to Chr($7E) do
    if (C in LegalTokenChars) then
      CheckEquals('ab' + C + 'cd',
                  QuoteStringIfNecessary('ab' + C + 'cd'),
                  'ab' + C + 'cd')
    else
      CheckEquals('"ab' + C + 'cd"',
                  QuoteStringIfNecessary('ab' + C + 'cd'),
                  'ab' + C + 'cd');
end;

procedure TestFunctions.TestQValueToStr;
begin
  CheckEquals('0',     QValueToStr(0),    'QValueToStr(0)');
  CheckEquals('0.001', QValueToStr(1),    'QValueToStr(1)');
  CheckEquals('0.01',  QValueToStr(10),   'QValueToStr(10)');
  CheckEquals('0.1',   QValueToStr(100),  'QValueToStr(100)');
  CheckEquals('0.666', QValueToStr(666),  'QValueToStr(666)');
  CheckEquals('1',     QValueToStr(1000), 'QValueToStr(1000)');
end;

procedure TestFunctions.TestReadDigit;
var
  S: String;
begin
  S := '';
  CheckEquals('',
              ReadDigit(S),
              'Empty string');
  CheckEquals('',
              S,
              'Remainder of empty string');

  S := '12345678901234567890';
  CheckEquals('12345678901234567890',
              ReadDigit(S), '12345678901234567890');
  CheckEquals('',
              S,
              '12345678901234567890');

  S := '1234abcd';
  CheckEquals('1234',
              ReadDigit(S),
              '1324abcd');
  CheckEquals('abcd',
              S,
              '1234abcd');

  S := 'abcd';
  CheckEquals('',
              ReadDigit(S), 'abcd');
  CheckEquals('abcd',
              S,
              'abcd');
end;

procedure TestFunctions.TestStrToQValue;
begin
  CheckEquals(0,    StrToQValue('0'),     'StrToQValue(''0'')');
  CheckEquals(0,    StrToQValue('0.0'),   'StrToQValue(''0.0'')');
  CheckEquals(0,    StrToQValue('0.00'),  'StrToQValue(''0.00'')');
  CheckEquals(0,    StrToQValue('0.000'), 'StrToQValue(''0.000'')');
  CheckEquals(666,  StrToQValue('0.666'), 'StrToQValue(''0.666'')');
  CheckEquals(700,  StrToQValue('0.7'),   'StrToQValue(''0.7'')');
  CheckEquals(1000, StrToQValue('1'),     'StrToQValue(''1'')');
  CheckEquals(1000, StrToQValue('1.0'),   'StrToQValue(''1.0'')');
  CheckEquals(1000, StrToQValue('1.00'),  'StrToQValue(''1.00'')');
  CheckEquals(1000, StrToQValue('1.000'), 'StrToQValue(''1.000'')');

  try
    StrToQValue('.');
    Fail('Failed to bail out on malformed q (.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.');
    Fail('Failed to bail out on malformed q (0.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0. 0');
    Fail('Failed to bail out on malformed q (0. 0)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0. 0', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.');
    Fail('Failed to bail out on malformed q (1.');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['1.', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0000');
    Fail('Failed to bail out on too many digits (0.0000)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.0000', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.0123');
    Fail('Failed to bail out on too many digits (0.0123)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.0123', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.1234');
    Fail('Failed to bail out on too many digits (0.1234)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.1234', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('0.a');
    Fail('Failed to bail out on letters');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['0.a', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('1.1');
    Fail('Failed to bail out on number too big (1.1)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['1.1', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('3');
    Fail('Failed to bail out on number too big (3)');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['3', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;

  try
    StrToQValue('');
    Fail('Failed to bail out on empty string');
  except
    on E: EConvertError do
      CheckEquals(Format(ConvertErrorMsg, ['', 'TIdSipQValue']),
                  E.Message,
                  'Unexpected exception');
  end;
end;

procedure TestFunctions.TestStrToQValueDef;
begin
  CheckEquals(666, StrToQValueDef('', 666), '''''');
end;

//******************************************************************************
//* TIdSipParameterTestCase                                                    *
//******************************************************************************
//* TestTIdSipParameter Public methods *****************************************

procedure TIdSipParameterTestCase.SetUp;
begin
  inherited SetUp;

  Self.P := Self.CreateParameter;
end;

procedure TIdSipParameterTestCase.TearDown;
begin
  Self.P.Free;

  inherited TearDown;
end;

//* TIdSipParameterTestCase Protected methods **********************************

function TIdSipParameterTestCase.CreateParameter: TIdSipParameter;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'CreateParameter');
end;

//* TIdSipParameterTestCase Published methods **********************************

procedure TIdSipParameterTestCase.TestAsStringParameterHasNoValue;
const
  ParamName = 'foo';
begin
  Self.P.Name := ParamName;

  CheckEquals(Self.P.Name,
              Self.P.AsString,
              'Parameter mustn''t print the equals sign when it has no value');
end;

procedure TIdSipParameterTestCase.TestEquals;
const
  OtherName  = 'baz';
  OtherValue = 'quaax';
var
  Other: TIdSipParameter;
begin
  Other := Self.CreateParameter;
  try
    Self.P.Name  := 'foo';
    Self.P.Value := 'bar';
    Other.Name   := OtherName;
    Other.Value  := OtherValue;

    Check(not Other.Equals(Self.P), 'Different name and value');

    Other.Value := Self.P.Value;
    Check(not Other.Equals(Self.P), 'Different name, same value');

    Other.Name := Self.P.Name;
    Check(Other.Equals(Self.P), 'Same name, same value');

    Other.Name := OtherName;
    Check(not Other.Equals(Self.P), 'Same name, different value');

    Other.Name := Uppercase(Self.P.Name);
    Other.Value := Uppercase(Self.P.Value);
    Check(Other.Equals(Self.P), 'Same name, same value, different case');

    Self.P.Value := '';
    Other.Value := Self.P.Value;
    Check(Other.Equals(Self.P), 'Same name, different case, no value');
  finally
    Other.Free;
  end;
end;

procedure TIdSipParameterTestCase.TestName;
const
  NewName = 'foo';
begin
  CheckNotEquals(NewName,
                 Self.P.Name,
                 'Sanity check: an uninitialised parameter should a name of '
               + 'the empty string');
  Self.P.Name := NewName;
  CheckEquals(NewName,
              Self.P.Name,
              'Name not set');
end;

procedure TIdSipParameterTestCase.TestValue;
begin
  Fail('You must override this method in ' + Self.ClassName);
end;

//******************************************************************************
//* TestTIdSipParameter                                                        *
//******************************************************************************
//* TestTIdSipParameter Protected methods **************************************

function TestTIdSipParameter.CreateParameter: TIdSipParameter;
begin
  Result := TIdSipParameter.Create;
end;

//* TestTIdSipParameter Published methods **************************************

procedure TestTIdSipParameter.TestAsHeaderParameter;
const
  ParamName     = 'foo';
  ParamValue    = 'bar';
begin
  Self.P.Name  := ParamName;
  Self.P.Value := ParamValue;

  CheckEquals(ParamName + '=' + ParamValue,
              Self.P.AsHeaderParameter,
              'Normal parameter');

  Self.P.Value := '\"/';
  CheckEquals(ParamName + '="\\\"/"',
              Self.P.AsHeaderParameter,
              'Parameter with characters needing escaping');
end;

procedure TestTIdSipParameter.TestAsString;
const
  ParamName     = 'foo';
  ParamValue    = 'bar';
  ParamAsString = ParamName + '=' + ParamValue;
begin
  Self.P.Name  := ParamName;
  Self.P.Value := ParamValue;

  CheckEquals(ParamAsString,
              Self.P.AsString,
              'AsString');
end;

procedure TestTIdSipParameter.TestAsStringWithEscapedCharacters;
const
  EscapedCharValue = 'bar;';
  ParamFoo         = 'foo';
begin
  Self.P.Name  := ParamFoo;
  Self.P.Value := EscapedCharValue;

  CheckEquals(Self.P.Name + '=' + TIdSipUri.ParameterEncode(Self.P.Value),
              Self.P.AsString,
              'Parameter not correctly encoded');
end;

procedure TestTIdSipParameter.TestAsUriParameter;
const
  ParamName     = 'foo';
  ParamValue    = 'bar';
begin
  Self.P.Name  := ParamName;
  Self.P.Value := ParamValue;

  CheckEquals(ParamName + '=' + ParamValue,
              Self.P.AsUriParameter,
              'Normal parameter');

  Self.P.Value := '\"/';
  CheckEquals(ParamName + '=%5C%22/',
              Self.P.AsUriParameter,
              'Parameter with characters needing escaping');
end;

procedure TestTIdSipParameter.TestValue;
const
  NewValue = 'foo';
begin
  CheckNotEquals(NewValue,
                 Self.P.Value,
                 'Sanity check: an uninitialised parameter should a value of '
               + 'the empty string');
  Self.P.Value := NewValue;
  CheckEquals(NewValue,
              Self.P.Value,
              'Value not set');
end;

//******************************************************************************
//* TestTIdSipQuotedStringParameter                                            *
//******************************************************************************
//* TestTIdSipQuotedStringParameter Protected methods **************************

function TestTIdSipQuotedStringParameter.CreateParameter: TIdSipParameter;
begin
  Result := TIdSipQuotedStringParameter.Create;
end;

//* TestTIdSipQuotedStringParameter Published methods **************************

procedure TestTIdSipQuotedStringParameter.TestAsHeaderParameter;
const
  ParamValue = 'bar';
begin
  Self.P.Name := 'foo';
  Self.P.Value := ParamValue;

  CheckEquals(Self.P.Name + '="'
            + EncodeQuotedStr(Self.P.Value) + '"',
              Self.P.AsHeaderParameter,
              'quoted-string incorrectly encoded');
end;

procedure TestTIdSipQuotedStringParameter.TestAsString;
const
  ParamValue = '"bar"';
begin
  Self.P.Name := 'foo';
  Self.P.Value := ParamValue;

  CheckEquals(Self.P.Name + '="'
            + EncodeQuotedStr(Self.P.Value) + '"',
              Self.P.AsString,
              'quoted-string incorrectly encoded');
end;

procedure TestTIdSipQuotedStringParameter.TestValue;
const
  ParamValue = '"bar"';
begin
  Self.P.Name  := 'foo';
  Self.P.Value := ParamValue;

  CheckEquals(ParamValue,
              Self.P.Value,
              'Parameter needs to keep its quotes');
end;

//******************************************************************************
//* TIdSipParametersTestCase                                                   *
//******************************************************************************
//* TIdSipParametersTestCase Public methods ************************************

procedure TIdSipParametersTestCase.SetUp;
begin
  inherited SetUp;

  Self.Params := Self.CreateParameters;
end;

procedure TIdSipParametersTestCase.TearDown;
begin
  Self.Params.Free;

  inherited TearDown;
end;

//* TIdSipParametersTestCase Protected methods *********************************

function TIdSipParametersTestCase.CreateParameters: TIdSipParameters;
begin
  Result := nil;
  Fail(Self.ClassName + ' MUST override CreateParameters');
end;

//* TIdSipParametersTestCase Published methods *********************************

procedure TIdSipParametersTestCase.TestAddParamTypeDependsOnNameParameter;
begin
  CheckEquals(TIdSipParameter.ClassName,
              Self.Params.AddParam('unknownparam', '').ClassName,
              'unknownparam type');
  CheckEquals(TIdSipQuotedStringParameter.ClassName,
              Self.Params.AddParam(GruuParam, '').ClassName,
              GruuParam + ' type');
end;

procedure TIdSipParametersTestCase.TestAddParamAndHasParam;
begin
  Check(not Self.Params.HasParameter(GruuParam),
        'Sanity check: an empty parameter list shouldn''t have a "gruu" parameter');

  Self.Params.AddParam(GruuParam, 'sip:wintermute@tessier-ashpool.co.luna;opaque=foofoo');

  Check(Self.Params.HasParameter(GruuParam),
        'Parameter not added');
end;

procedure TIdSipParametersTestCase.TestAdd;
const
  Param1      = 'foo';
  Param2      = 'bar';
  ParamValue1 = '1';
  ParamValue2 = '2';
var
  Other: TIdSipParameters;
begin
  Self.Params.AddParam(Param1, ParamValue1);
  Self.Params.AddParam(Param2, ParamValue2);

  Other := TIdSipParameters.Create;
  try
    Other.Add(Self.Params);

    Check(Other.Equals(Self.Params),
          'Add didn''t add all parameters');
  finally
    Other.Free;
  end;
end;

procedure TIdSipParametersTestCase.TestAssign;
const
  Param1      = 'foo';
  Param2      = 'bar';
  ParamValue1 = '1';
  ParamValue2 = '2';
var
  Other: TIdSipParameters;
begin
  Self.Params.AddParam(Param1, ParamValue1);
  Self.Params.AddParam(Param2, ParamValue2);

  Other := TIdSipParameters.Create;
  try
    Other.Assign(Self.Params);

    Check(Other.Equals(Self.Params),
          'Assign didn''t add all parameters');
  finally
    Other.Free;
  end;
end;

procedure TIdSipParametersTestCase.TestAssignToNonEmptyList;
const
  Param1      = 'foo';
  Param2      = 'bar';
  ParamValue1 = '1';
  ParamValue2 = '2';
var
  Other: TIdSipParameters;
begin
  Other := TIdSipParameters.Create;
  try
    Self.Params.AddParam(Param1, ParamValue1);
    Other.AddParam(Param2, ParamValue2);
      
    Other.Assign(Self.Params);

    Check(not Other.HasParameter(Param2),
          'Assign didn''t clear out existing parameters'); 
    Check(Other.Equals(Self.Params),
          'Assign didn''t add all parameters');
  finally
    Other.Free;
  end;
end;

procedure TIdSipParametersTestCase.TestAsString;
begin
  Fail(Self.ClassName + ' MUST override TestAsString');
end;

procedure TIdSipParametersTestCase.TestClear;
begin
  Self.Params.AddParam(GruuParam, 'sip:wintermute@tessier-ashpool.co.luna;opaque=foofoo');
  Self.Params.AddParam(SipInstanceParam, '<urn:foo:bar>');

  Self.Params.Clear;
  Check(not Self.Params.HasParameter(GruuParam),
        'Clear didn''t remove the "' + GruuParam + '" parameter');
  Check(not Self.Params.HasParameter(SipInstanceParam),
        'Clear didn''t remove the "' + SipInstanceParam + '" parameter');
end;

procedure TIdSipParametersTestCase.TestCount;
begin
  CheckEquals(0, Self.Params.Count, 'Empty list');

  Self.Params.AddParam('foo', '');
  CheckEquals(1, Self.Params.Count, 'One Add');

  Self.Params.AddParam('bar', '');
  CheckEquals(2, Self.Params.Count, 'Two Adds');
end;

procedure TIdSipParametersTestCase.TestDoubleAdd;
const
  Param    = 'foo';
  OldValue = 'bar';
  NewValue = 'baz';
begin
  Self.Params.AddParam(Param, OldValue);
  Self.Params.AddParam(Param, NewValue);

  CheckEquals(OldValue,
              Self.Params.ParamValue(Param),
              'New value overwrite the old value');
  Check(Self.Params.HasDuplicatedParameter(Param),
        'Duplicate param not added, even though this typically renders the '
      + 'parameter list malformed');
end;

procedure TIdSipParametersTestCase.TestEquals;
const
  Param1      = 'foo';
  Param2      = 'bar';
  Param3      = 'baz';
  Param4      = 'quaax';
  ParamValue1 = '1';
  ParamValue2 = '2';
  ParamValue3 = '3';
  ParamValue4 = '4';
var
  Other: TIdSipParameters;
begin
  Other := TIdSipParameters.Create;
  try
    Check(Self.Params.Equals(Other), 'Params <> Other; empty lists');
    Check(Other.Equals(Self.Params), 'Other <> Params; empty lists');

    Self.Params.AddParam(Param1, ParamValue1);
    Self.Params.AddParam(Param2, ParamValue2);

    Other.AddParam(Param1, ParamValue1);
    Other.AddParam(Param2, ParamValue2);

    Check(Self.Params.Equals(Other), 'Params <> Other; non-empty, identical lists');
    Check(Other.Equals(Self.Params), 'Other <> Params; non-empty, identical lists');

    Self.Params.AddParam(Param3, ParamValue3);
    Check(not Self.Params.Equals(Other),
          'Params = Other; Params has an extra parameter');
    Check(not Other.Equals(Self.Params),
          'Other = Params; Params has an extra parameter');

    Other.AddParam(Param3, ParamValue3);
    Check(Self.Params.Equals(Other),
          'Params <> Other; Other has the extra parameter too');
    Check(Other.Equals(Self.Params),
          'Other <> Params; Other has the extra parameter too');

    Other.AddParam(Param4, ParamValue4);
    Check(not Self.Params.Equals(Other),
          'Params = Other; Other has an extra parameter');
    Check(not Other.Equals(Self.Params),
          'Other = Params; Other has an extra parameter');
  finally
    Other.Free;
  end;
end;

procedure TIdSipParametersTestCase.TestHasParam;
begin
  Self.Params.AddParam(GruuParam, 'sip:wintermute@tessier-ashpool.co.luna;opaque=foofoo');

  Check(Self.Params.HasParameter(GruuParam),
        'Actual parameter name');
  Check(Self.Params.HasParameter(Uppercase(GruuParam)),
        'Uppercased parameter name');
end;

procedure TIdSipParametersTestCase.TestParameterTypes;
begin
  CheckEquals(TIdSipQuotedStringParameter.ClassName,
              Self.Params.AddParam(GruuParam, '').ClassName,
              GruuParam);
  CheckEquals(TIdSipQuotedStringParameter.ClassName,
              Self.Params.AddParam(SipInstanceParam, '').ClassName,
              SipInstanceParam);
end;

procedure TIdSipParametersTestCase.TestParamValue;
const
  ParamName  = 'foo';
  ParamValue = 'bar';
begin
  Self.Params.AddParam(ParamName + '1', ParamValue + '1');
  Self.Params.AddParam(ParamName,       ParamValue);
  Self.Params.AddParam(ParamName + '2', ParamValue + '2');

  CheckEquals(ParamValue,
              Self.Params.ParamValue(ParamName),
              'ParamValue');

  CheckEquals('',
              Self.Params.ParamValue('unknownparam'),
              'Non-existent parameter');
end;

procedure TIdSipParametersTestCase.TestRemoveParameter;
const
  ParamName  = 'foo';
  ParamValue = 'bar';
begin
  Self.Params.AddParam(ParamName, ParamValue);

  Self.Params.RemoveParameter(ParamName);
  Check(not Self.Params.HasParameter(ParamName),
        'Parameter not removed');
end;

procedure TIdSipParametersTestCase.TestRemoveParameterParameterMissing;
const
  ParamName  = 'foo';
begin
  Check(not Self.Params.HasParameter(ParamName),
        'Sanity check: the parameter should be empty');

  // Check that trying to remove a non-extant parameter doesn't raise an
  // exception.
  Self.Params.RemoveParameter(ParamName);
end;

procedure TIdSipParametersTestCase.TestIntersectionEquals;
const
  Param1 = 'foo'; ParamValue1 = '1';
  Param2 = 'bar'; ParamValue2 = '2';
var
  Other: TIdSipParameters;
begin
  Other := TIdSipParameters.Create;
  try
    Check(Self.Params.IntersectionEquals(Other), 'Empty lists (Params)');
    Check(Other.IntersectionEquals(Self.Params), 'Empty lists (Other');

    Self.Params.AddParam(Param1, ParamValue1);
    Check(Self.Params.IntersectionEquals(Other), 'Other is empty (Params)');
    Check(Other.IntersectionEquals(Self.Params), 'Other is empty (Other)');

    Other.AddParam(Param2, ParamValue2);
    Check(Self.Params.IntersectionEquals(Other),
          'Parameter intersection empty (Params)');
    Check(Other.IntersectionEquals(Self.Params),
          'Parameter intersection empty (Other)');

    Other.AddParam(Param1, ParamValue1 + '1');
    Check(not Self.Params.IntersectionEquals(Other),
          'Other has a differing param (Params)');
    Check(not Other.IntersectionEquals(Self.Params),
          'Other has a differing param (Other)');
  finally
    Other.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipHeaderParameters                                                 *
//******************************************************************************
//* TestTIdSipHeaderParameters Protected methods *******************************

function TestTIdSipHeaderParameters.CreateParameters: TIdSipParameters;
begin
  Result := TIdSipHeaderParameters.Create;
end;

//* TestTIdSipHeaderParameters Published methods *******************************

procedure TestTIdSipHeaderParameters.TestAsString;
begin
  Self.Params['branch'] := 'z9hG4bK776asdhds';
  Self.Params['ttl']    := '5';
  Self.Params['foo']    := 'foo bar\';

  CheckEquals(';branch=z9hG4bK776asdhds;ttl=5;foo="foo bar\\"',
              Self.Params.AsString,
              'AsString');
end;

procedure TestTIdSipHeaderParameters.TestIsMalformed;
begin
  Check(not Self.Params.IsMalformed, 'The empty set of headers is well-formed');

  Self.Params[SubjectHeaderFull] := 'This is a subject';
  Check(not Self.Params.IsMalformed, 'One well-formed header');

  Self.Params[AllowHeader] := '';
  Check(not Self.Params.IsMalformed, 'One well-formed header, one valueless header');

  // An invalid URI.
  Self.Params[ContactHeaderFull] := 'sipp:127.0.0.1';
  Check(Self.Params.IsMalformed, 'One well-formed header, one valueless header, one malformed header');

  Self.Params[ContactHeaderFull] := 'sip:127.0.0.1';
  Check(not Self.Params.IsMalformed, 'Two well-formed headers, one valueless header');
end;

procedure TestTIdSipHeaderParameters.TestParse;
begin
  Self.Params.Parse('');
  CheckEquals(0, Self.Params.Count, 'Empty string means no parameters');

  Self.Params.AddParam('foo', 'bar');
  Self.Params.Parse('');
  CheckEquals(0,
              Self.Params.Count,
              'Parse removes any old parameters');
end;

procedure TestTIdSipHeaderParameters.TestParseMalformedQuotedString;
begin
  try
    Self.Params.Parse(';gruu="sip:wintermute@tessier-ashpool.co.luna');
    Fail('Failed to bail out on parameter list with malformed quoted-string: no closing quote');
  except
    on EBadHeader do;
  end;

  try
    Self.Params.Parse(';gruu="sip:wintermute@tessier-ashpool.co.luna\"');
    Fail('Failed to bail out on parameter list with malformed quoted-string: closing quote is escaped');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipHeaderParameters.TestParseMissingValue;
begin
  try
    Self.Params.Parse(';q=');
    Fail('Failed to bail out on parameter with missing value');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipHeaderParameters.TestParseMultipleParameters;
const
  Param1 = 'foo'; ParamValue1 = '1';
  Param2 = 'bar'; ParamValue2 = '2';
  Param3 = 'baz'; ParamValue3 = '3';
begin
  Self.Params.Parse(Param1 + '=' + ParamValue1 + ';'
                                    + Param2 + '=' + ParamValue2 + ';'
                                    + Param3 + '=' + ParamValue3);

  CheckEquals(3, Self.Params.Count, 'Number of parameters');

  Check(Self.Params.HasParameter(Param1), '"' + Param1 + '" not added');
  CheckEquals(ParamValue1, Self.Params[Param1], '"' + Param1 + '" value');

  Check(Self.Params.HasParameter(Param2), '"' + Param2 + '" not added');
  CheckEquals(ParamValue2, Self.Params[Param2], '"' + Param2 + '" value');

  Check(Self.Params.HasParameter(Param3), '"' + Param3 + '" not added');
  CheckEquals(ParamValue3, Self.Params[Param3], '"' + Param3 + '" value');
end;

procedure TestTIdSipHeaderParameters.TestParseNoNameNoValue;
begin
  try
    Self.Params.Parse(';;,;');
    Fail('Failed to bail out of parsing malformed parameter list');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipHeaderParameters.TestParseQuotedString;
const
  ParamName  = 'foo';
  ParamValue = '\"<urn:foo:bar>\"';
var
  DecodedParamValue: String;
begin
  Check(DecodeQuotedStr(ParamValue, DecodedParamValue),
        'Sanity check: ParamValue must contain a valid quoted-string');

  Self.Params.Parse(';' + ParamName + '="' + ParamValue + '"');

  Check(Self.Params.HasParameter(ParamName),
        '"' + ParamName + '" not added');
  CheckEquals(DecodedParamValue,
              Self.Params[ParamName],
              '"' + ParamName + '" value');
  CheckEquals(1,
              Self.Params.Count,
              'Number of parameters');
end;

procedure TestTIdSipHeaderParameters.TestParseQuotedStringContainsSemicolon;
const
  ParamName  = 'gruu';
  ParamValue = 'sip:wintermute@tessier-ashpool.co.luna;opaque=foo';
begin
  Self.Params.Parse(';' + ParamName + '="' + ParamValue + '"');

  Check(Self.Params.HasParameter(ParamName),
        '"' + ParamName + '" not added');
  CheckEquals(ParamValue,
              Self.Params[ParamName],
              '"' + ParamName + '" value');
  CheckEquals(1,
              Self.Params.Count,
              'Number of parameters');
end;

procedure TestTIdSipHeaderParameters.TestParseSimple;
const
  ParamName  = 'foo';
  ParamValue = 'bar';
begin
  Self.Params.Parse(';' + ParamName + '=' + ParamValue);

  Check(Self.Params.HasParameter(ParamName),
        '"' + ParamName + '" not added');
  CheckEquals(ParamValue,
              Self.Params[ParamName],
              '"' + ParamName + '" value');
  CheckEquals(1,
              Self.Params.Count,
              'Number of parameters');
end;

procedure TestTIdSipHeaderParameters.TestParseTorture;
const
  TortureParams = ';gruu="\"Wintermute\" <sip:wintermute@tessier-ashpool.co.luna";foo;bar="\\;\"";foo';
begin
  Self.Params.Parse(TortureParams);

  Check(Self.Params.HasParameter('gruu'),
        '"gruu" parameter not added');
  CheckEquals('"Wintermute" <sip:wintermute@tessier-ashpool.co.luna',
              Self.Params['gruu'],
              '"gruu" parameter value');
  Check(Self.Params.HasParameter('foo'),
        '"foo" parameter not added');
  Check(Self.Params.HasDuplicatedParameter('foo'),
        'Duplicate (and erroneous) "foo" parameter not added');
  CheckEquals('',
              Self.Params['foo'],
              '"foo" parameter value');
  Check(Self.Params.HasParameter('bar'),
        '"bar" parameter not added');
   CheckEquals('\;"',
               Self.Params['bar'],
               '"bar" parameter');
end;

procedure TestTIdSipHeaderParameters.TestParseValuelessParameter;
const
  ParamName = 'foo';
begin
  Self.Params.Parse(';' + ParamName);

  Check(Self.Params.HasParameter(ParamName),
        '"' + ParamName + '" not added');
  CheckEquals('',
              Self.Params[ParamName],
              '"' + ParamName + '" value');
  CheckEquals(1,
              Self.Params.Count,
              'Number of parameters');
end;

procedure TestTIdSipHeaderParameters.TestParseWithWhitespace;
begin
  Self.Params.Parse(' ; foo = bar ');

  Check(Self.Params.HasParameter('foo'),
        '"foo" parameter not added');
  CheckEquals('bar',
              Self.Params['foo'],
              '"foo" parameter value');
end;

//******************************************************************************
//* TestTIdSipUriParameters                                                    *
//******************************************************************************
//* TestTIdSipUriParameters Protected methods **********************************

function TestTIdSipUriParameters.CreateParameters: TIdSipParameters;
begin
  Result := TIdSipUriParameters.Create;
end;

//* TestTIdSipUriParameters Published methods **********************************

procedure TestTIdSipUriParameters.TestAsString;
begin
  Self.Params['branch'] := 'z9hG4bK776asdhds';
  Self.Params['ttl']    := '5';
  Self.Params['foo']    := 'foo bar\';

  CheckEquals(';branch=z9hG4bK776asdhds;ttl=5;foo=foo%20bar%5C',
              Self.Params.AsString,
              'AsString');
end;

procedure TestTIdSipUriParameters.TestIsMalformed;
begin
  Check(not Self.Params.IsMalformed, 'The empty set of parameters is not malformed');
  Self.Params['foo'] := 'bar';
  Check(not Self.Params.IsMalformed, 'One well-formed parameter');

  Self.Params['baz'] := ';';
  Check(Self.Params.IsMalformed, 'One well-formed parameter, one malformed parameter');

  Self.Params['baz'] := 'quaax';
  Check(not Self.Params.IsMalformed, 'Two well-formed parameters');

  Self.Params['empty'] := '';
  Check(not Self.Params.IsMalformed, 'Two well-formed parameters; a valueless parameter');
end;

//******************************************************************************
//* THeaderTestCase                                                            *
//******************************************************************************
//* THeaderTestCase Public methods *********************************************

function THeaderTestCase.HeaderType: TIdSipHeaderClass;
begin
  raise Exception.Create(Self.ClassName + ' must override HeaderType');
  Result := nil;
end;

procedure THeaderTestCase.SetUp;
begin
  inherited SetUp;

  Self.Header := Self.HeaderType.Create;
end;

procedure THeaderTestCase.TearDown;
begin
  Self.Header.Free;

  inherited TearDown;
end;

//* THeaderTestCase Published methods ******************************************

procedure THeaderTestCase.TestIsContact;
begin
  Check(not Self.Header.IsContact,
        Self.Header.ClassName + ' claims it''s a Contact header');
end;

procedure THeaderTestCase.TestValue;
begin
  try
    CheckEquals('',
                Self.Header.Value,
                'Value-less header');

    Self.Header.Name := 'Foo';
    CheckEquals('',
                Self.Header.Value,
                'Value-less header after name''s set');

    Self.Header.Value := 'Fighters';
    CheckEquals('Fighters',
                Self.Header.Value,
                'Value-ful header');

    Self.Header.Params['branch'] := 'haha';
    CheckEquals('Fighters',
                Self.Header.Value,
                'Value-ful header with a param');

    Self.Header.Params['ttl'] := 'eheh';
    CheckEquals('Fighters',
                Self.Header.Value,
                'Value-ful header with multiple params');

    Self.Header.Value := 'Fluffy';
    CheckEquals(0,
                Self.Header.ParamCount,
                'Didn''t clear out old params');
  except
    on E: Exception do begin
      raise ExceptClass(E.ClassType).Create(Self.Header.ClassName
                                                + ' ' + E.Message);
    end;
  end;
end;

//******************************************************************************
//* TestTIdSipHeader                                                           *
//******************************************************************************
//* TestTIdSipHeader Public methods ********************************************

procedure TestTIdSipHeader.SetUp;
begin
  inherited SetUp;

  Self.H := Self.Header;
end;

//* TestTIdSipHeader Protected methods *****************************************

function TestTIdSipHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipHeader;
end;

//* TestTIdSipHeader Published methods *****************************************

procedure TestTIdSipHeader.TestAsString;
begin
  CheckEquals(': ', Self.H.AsString, 'AsString with no set properties');

  Self.H.Name := 'Foo';
  Self.H.Value := 'Fighters';
  CheckEquals('Foo: Fighters', Self.H.AsString, 'Foo: Fighters');

  Self.H.Params['tag'] := 'haha';
  CheckEquals('Foo: Fighters;tag=haha',
              Self.H.AsString,
              '''Foo: Fighters'' with tag');

  Self.H.Params['hidden'] := '';
  CheckEquals('Foo: Fighters;tag=haha;hidden',
              Self.H.AsString,
              '''Foo: Fighters'' with tag & hidden');
end;

procedure TestTIdSipHeader.TestAssignCopiesParseInfo;
var
  C:    TIdSipNumericHeader;
  NewC: TIdSipNumericHeader;
begin
  C := TIdSipNumericHeader.Create;
  try
    NewC := TIdSipNumericHeader.Create;
    try
      C.Value := 'a';

      Check(C.IsMalformed, 'Numeric header not marked as malformed');

      NewC.Assign(C);
      Check(C.IsMalformed = NewC.IsMalformed, 'IsMalformed not copied');
      CheckEquals(C.ParseFailReason,
                  NewC.ParseFailReason,
                  'ParseFailReason not copied');
      CheckEquals(C.UnparsedValue,
                  NewC.UnparsedValue,
                  'UnparsedValue not copied');
    finally
      NewC.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TestTIdSipHeader.TestCanonicaliseName;
begin
  CheckEquals('', TIdSipHeader.CanonicaliseName(''), 'Empty string');
  CheckEquals('New-Header', TIdSipHeader.CanonicaliseName('New-Header'), 'New-Header');
  CheckEquals('new-header', TIdSipHeader.CanonicaliseName('new-header'), 'new-header');

  CheckEquals(AcceptHeader, TIdSipHeader.CanonicaliseName('accept'),     'accept');
  CheckEquals(AcceptHeader, TIdSipHeader.CanonicaliseName('Accept'),     'Accept');
  CheckEquals(AcceptHeader, TIdSipHeader.CanonicaliseName(AcceptHeader), 'AcceptHeader constant');

  CheckEquals(AcceptEncodingHeader, TIdSipHeader.CanonicaliseName('accept-encoding'),    'accept-encoding');
  CheckEquals(AcceptEncodingHeader, TIdSipHeader.CanonicaliseName('Accept-Encoding'),    'Accept-Encoding');
  CheckEquals(AcceptEncodingHeader, TIdSipHeader.CanonicaliseName(AcceptEncodingHeader), 'AcceptEncodingHeader constant');

  CheckEquals(AcceptLanguageHeader, TIdSipHeader.CanonicaliseName('accept-language'),    'accept-language');
  CheckEquals(AcceptLanguageHeader, TIdSipHeader.CanonicaliseName('Accept-Language'),    'Accept-Language');
  CheckEquals(AcceptLanguageHeader, TIdSipHeader.CanonicaliseName(AcceptLanguageHeader), 'AcceptLanguageHeader constant');

  CheckEquals(AlertInfoHeader, TIdSipHeader.CanonicaliseName('alert-info'),    'alert-info');
  CheckEquals(AlertInfoHeader, TIdSipHeader.CanonicaliseName('Alert-Info'),    'Alert-Info');
  CheckEquals(AlertInfoHeader, TIdSipHeader.CanonicaliseName(AlertInfoHeader), 'AlertInfoHeader constant');

  CheckEquals(AllowHeader, TIdSipHeader.CanonicaliseName('allow'),     'allow');
  CheckEquals(AllowHeader, TIdSipHeader.CanonicaliseName('Allow'),     'Allow');
  CheckEquals(AllowHeader, TIdSipHeader.CanonicaliseName(AllowHeader), 'AllowHeader constant');

  CheckEquals(AuthenticationInfoHeader, TIdSipHeader.CanonicaliseName('authentication-info'),    'authentication-info');
  CheckEquals(AuthenticationInfoHeader, TIdSipHeader.CanonicaliseName('Authentication-Info'),    'Authentication-Info');
  CheckEquals(AuthenticationInfoHeader, TIdSipHeader.CanonicaliseName(AuthenticationInfoHeader), 'AuthenticationInfoHeader constant');

  CheckEquals(AuthorizationHeader, TIdSipHeader.CanonicaliseName('authorization'),     'authorization');
  CheckEquals(AuthorizationHeader, TIdSipHeader.CanonicaliseName('Authorization'),     'Authorization');
  CheckEquals(AuthorizationHeader, TIdSipHeader.CanonicaliseName(AuthorizationHeader), 'AuthorizationHeader constant');

  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName('call-ID'),         'call-ID');
  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName('Call-ID'),         'Call-ID');
  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName('i'),               'i');
  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName('I'),               'I');
  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName(CallIDHeaderFull),  'CallIDHeaderFull constant');
  CheckEquals(CallIDHeaderFull, TIdSipHeader.CanonicaliseName(CallIDHeaderShort), 'CallIDHeaderShort constant');

  CheckEquals(CallInfoHeader, TIdSipHeader.CanonicaliseName('call-info'),     'call-info');
  CheckEquals(CallInfoHeader, TIdSipHeader.CanonicaliseName('Call-Info'),     'Call-Info');
  CheckEquals(CallInfoHeader, TIdSipHeader.CanonicaliseName(CallInfoHeader), 'CallInfoHeader constant');

  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName('contact'),          'contact');
  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName('Contact'),          'Contact');
  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName('m'),                'm');
  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName('M'),                'M');
  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName(ContactHeaderFull),  'ContactHeaderFull constant');
  CheckEquals(ContactHeaderFull, TIdSipHeader.CanonicaliseName(ContactHeaderShort), 'ContactHeaderShort constant');

  CheckEquals(ContentDispositionHeader, TIdSipHeader.CanonicaliseName('content-disposition'),    'content-disposition');
  CheckEquals(ContentDispositionHeader, TIdSipHeader.CanonicaliseName('Content-Disposition'),    'Content-Disposition');
  CheckEquals(ContentDispositionHeader, TIdSipHeader.CanonicaliseName(ContentDispositionHeader), 'ContentDispositionHeader constant');

  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName('content-encoding'),         'content-encoding');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName('Content-Encoding'),         'Content-Encoding');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName('e'),                        'e');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName('E'),                        'E');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName(ContentEncodingHeaderFull),  'ContentEncodingHeaderFull constant');
  CheckEquals(ContentEncodingHeaderFull, TIdSipHeader.CanonicaliseName(ContentEncodingHeaderShort), 'ContentEncodingHeaderShort constant');

  CheckEquals(ContentLanguageHeader, TIdSipHeader.CanonicaliseName('content-language'),    'content-language');
  CheckEquals(ContentLanguageHeader, TIdSipHeader.CanonicaliseName('Content-Language'),    'Content-Language');
  CheckEquals(ContentLanguageHeader, TIdSipHeader.CanonicaliseName(ContentLanguageHeader), 'ContentLanguageHeader constant');

  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName('Content-Length'),         'Content-Length');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName('l'),                      'l');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName('L'),                      'L');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName(ContentLengthHeaderFull),  'ContentLengthHeaderFull constant');
  CheckEquals(ContentLengthHeaderFull, TIdSipHeader.CanonicaliseName(ContentLengthHeaderShort), 'ContentLengthHeaderShort constant');

  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName('content-type'),         'content-type');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName('Content-Type'),         'Content-Type');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName('c'),                    'c');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName('C'),                    'C');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName(ContentTypeHeaderFull),  'ContentTypeHeaderFull constant');
  CheckEquals(ContentTypeHeaderFull, TIdSipHeader.CanonicaliseName(ContentTypeHeaderShort), 'ContentTypeHeaderShort constant');

  CheckEquals(CSeqHeader, TIdSipHeader.CanonicaliseName('cseq'),     'cseq');
  CheckEquals(CSeqHeader, TIdSipHeader.CanonicaliseName('CSeq'),     'CSeq');
  CheckEquals(CSeqHeader, TIdSipHeader.CanonicaliseName(CSeqHeader), 'CSeqHeader constant');

  CheckEquals(DateHeader, TIdSipHeader.CanonicaliseName('date'),     'date');
  CheckEquals(DateHeader, TIdSipHeader.CanonicaliseName('Date'),     'Date');
  CheckEquals(DateHeader, TIdSipHeader.CanonicaliseName(DateHeader), 'DateHeader constant');

  CheckEquals(ErrorInfoHeader, TIdSipHeader.CanonicaliseName('error-info'),     'irror-info');
  CheckEquals(ErrorInfoHeader, TIdSipHeader.CanonicaliseName('Error-Info'),     'Error-Info');
  CheckEquals(ErrorInfoHeader, TIdSipHeader.CanonicaliseName(ErrorInfoHeader), 'ErrorInfoHeader constant');

  CheckEquals(ExpiresHeader, TIdSipHeader.CanonicaliseName('expires'),     'expires');
  CheckEquals(ExpiresHeader, TIdSipHeader.CanonicaliseName('Expires'),     'Expires');
  CheckEquals(ExpiresHeader, TIdSipHeader.CanonicaliseName(ExpiresHeader), 'ExpiresHeader constant');

  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName('from'),          'from');
  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName('From'),          'From');
  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName('f'),             'f');
  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName('F'),             'F');
  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName(FromHeaderFull),  'FromHeaderFull constant');
  CheckEquals(FromHeaderFull, TIdSipHeader.CanonicaliseName(FromHeaderShort), 'FromHeaderShort constant');

  CheckEquals(InReplyToHeader, TIdSipHeader.CanonicaliseName('in-reply-to'),   'in-reply-to');
  CheckEquals(InReplyToHeader, TIdSipHeader.CanonicaliseName('In-Reply-To'),   'In-Reply-To');
  CheckEquals(InReplyToHeader, TIdSipHeader.CanonicaliseName(InReplyToHeader), 'InReplyToHeader constant');

  CheckEquals(MaxForwardsHeader, TIdSipHeader.CanonicaliseName('max-forwards'),    'max-forwards');
  CheckEquals(MaxForwardsHeader, TIdSipHeader.CanonicaliseName('Max-Forwards'),    'Max-Forwards');
  CheckEquals(MaxForwardsHeader, TIdSipHeader.CanonicaliseName(MaxForwardsHeader), 'MaxForwardsHeader constant');

  CheckEquals(MIMEVersionHeader, TIdSipHeader.CanonicaliseName('mime-version'),    'mime-version');
  CheckEquals(MIMEVersionHeader, TIdSipHeader.CanonicaliseName('MIME-Version'),    'MIME-Version');
  CheckEquals(MIMEVersionHeader, TIdSipHeader.CanonicaliseName(MIMEVersionHeader), 'MIMEVersionHeader constant');

  CheckEquals(MinExpiresHeader, TIdSipHeader.CanonicaliseName('min-expires'),    'min-expires');
  CheckEquals(MinExpiresHeader, TIdSipHeader.CanonicaliseName('Min-Expires'),    'Min-Expires');
  CheckEquals(MinExpiresHeader, TIdSipHeader.CanonicaliseName(MinExpiresHeader), 'MinExpiresHeader constant');

  CheckEquals(OrganizationHeader, TIdSipHeader.CanonicaliseName('organization'),     'organization');
  CheckEquals(OrganizationHeader, TIdSipHeader.CanonicaliseName('Organization'),     'Organization');
  CheckEquals(OrganizationHeader, TIdSipHeader.CanonicaliseName(OrganizationHeader), 'OrganizationHeader constant');

  CheckEquals(PriorityHeader, TIdSipHeader.CanonicaliseName('priority'),     'priority');
  CheckEquals(PriorityHeader, TIdSipHeader.CanonicaliseName('Priority'),     'Priority');
  CheckEquals(PriorityHeader, TIdSipHeader.CanonicaliseName(PriorityHeader), 'PriorityHeader constant');

  CheckEquals(ProxyAuthenticateHeader, TIdSipHeader.CanonicaliseName('proxy-authenticate'),    'proxy-authenticate');
  CheckEquals(ProxyAuthenticateHeader, TIdSipHeader.CanonicaliseName('Proxy-Authenticate'),    'Proxy-Authenticate');
  CheckEquals(ProxyAuthenticateHeader, TIdSipHeader.CanonicaliseName(ProxyAuthenticateHeader), 'ProxyAuthenticateHeader constant');

  CheckEquals(ProxyAuthorizationHeader, TIdSipHeader.CanonicaliseName('proxy-authorization'),    'proxy-authorization');
  CheckEquals(ProxyAuthorizationHeader, TIdSipHeader.CanonicaliseName('Proxy-Authorization'),    'Proxy-Authorization');
  CheckEquals(ProxyAuthorizationHeader, TIdSipHeader.CanonicaliseName(ProxyAuthorizationHeader), 'ProxyAuthorizationHeader constant');

  CheckEquals(ProxyRequireHeader, TIdSipHeader.CanonicaliseName('proxy-require'),    'proxy-require');
  CheckEquals(ProxyRequireHeader, TIdSipHeader.CanonicaliseName('Proxy-Require'),    'Proxy-Require');
  CheckEquals(ProxyRequireHeader, TIdSipHeader.CanonicaliseName(ProxyRequireHeader), 'ProxyRequireHeader constant');

  CheckEquals(RecordRouteHeader, TIdSipHeader.CanonicaliseName('record-route'),    'record-route');
  CheckEquals(RecordRouteHeader, TIdSipHeader.CanonicaliseName('Record-Route'),    'Record-Route');
  CheckEquals(RecordRouteHeader, TIdSipHeader.CanonicaliseName(RecordRouteHeader), 'RecordRouteHeader constant');

  CheckEquals(ReplyToHeader, TIdSipHeader.CanonicaliseName('reply-to'),    'reply-to');
  CheckEquals(ReplyToHeader, TIdSipHeader.CanonicaliseName('Reply-To'),    'Reply-To');
  CheckEquals(ReplyToHeader, TIdSipHeader.CanonicaliseName(ReplyToHeader), 'ReplyToHeader constant');

  CheckEquals(RequireHeader, TIdSipHeader.CanonicaliseName('require'),     'require');
  CheckEquals(RequireHeader, TIdSipHeader.CanonicaliseName('Require'),     'Require');
  CheckEquals(RequireHeader, TIdSipHeader.CanonicaliseName(RequireHeader), 'RequireHeader constant');

  CheckEquals(RetryAfterHeader, TIdSipHeader.CanonicaliseName('retry-after'),    'retry-after');
  CheckEquals(RetryAfterHeader, TIdSipHeader.CanonicaliseName('Retry-After'),    'Retry-After');
  CheckEquals(RetryAfterHeader, TIdSipHeader.CanonicaliseName(RetryAfterHeader), 'RetryAfterHeader constant');

  CheckEquals(RouteHeader, TIdSipHeader.CanonicaliseName('route'),     'route');
  CheckEquals(RouteHeader, TIdSipHeader.CanonicaliseName('Route'),     'Route');
  CheckEquals(RouteHeader, TIdSipHeader.CanonicaliseName(RouteHeader), 'RouteHeader constant');

  CheckEquals(ServerHeader, TIdSipHeader.CanonicaliseName('server'),     'server');
  CheckEquals(ServerHeader, TIdSipHeader.CanonicaliseName('Server'),     'Server');
  CheckEquals(ServerHeader, TIdSipHeader.CanonicaliseName(ServerHeader), 'ServerHeader constant');

  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName('subject'),          'subject');
  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName('Subject'),          'Subject');
  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName('s'),                's');
  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName('S'),                'S');
  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName(SubjectHeaderFull),  'SubjectHeaderFull constant');
  CheckEquals(SubjectHeaderFull, TIdSipHeader.CanonicaliseName(SubjectHeaderShort), 'SubjectHeaderShort constant');

  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName('supported'),          'supported');
  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName('Supported'),          'Supported');
  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName('k'),                  'k');
  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName('K'),                  'K');
  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName(SupportedHeaderFull),  'SupportedHeaderFull constant');
  CheckEquals(SupportedHeaderFull, TIdSipHeader.CanonicaliseName(SupportedHeaderShort), 'SupportedHeaderShort constant');

  CheckEquals(TimestampHeader, TIdSipHeader.CanonicaliseName('timestamp'),     'timestamp');
  CheckEquals(TimestampHeader, TIdSipHeader.CanonicaliseName('Timestamp'),     'Timestamp');
  CheckEquals(TimestampHeader, TIdSipHeader.CanonicaliseName(TimestampHeader), 'TimestampHeader constant');

  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName('to'),          'to');
  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName('To'),          'To');
  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName('t'),           't');
  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName('T'),           'T');
  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName(ToHeaderFull),  'ToHeaderFull constant');
  CheckEquals(ToHeaderFull, TIdSipHeader.CanonicaliseName(ToHeaderShort), 'ToHeaderShort constant');

  CheckEquals(UnsupportedHeader, TIdSipHeader.CanonicaliseName('unsupported'),     'unsupported');
  CheckEquals(UnsupportedHeader, TIdSipHeader.CanonicaliseName('Unsupported'),     'Unsupported');
  CheckEquals(UnsupportedHeader, TIdSipHeader.CanonicaliseName(UnsupportedHeader), 'UnsupportedHeader constant');

  CheckEquals(UserAgentHeader, TIdSipHeader.CanonicaliseName('user-agent'),    'user-agent');
  CheckEquals(UserAgentHeader, TIdSipHeader.CanonicaliseName('User-Agent'),    'User-Agent');
  CheckEquals(UserAgentHeader, TIdSipHeader.CanonicaliseName(UserAgentHeader), 'UserAgentHeader constant');

  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName('via'),          'via');
  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName('Via'),          'Via');
  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName('v'),            'v');
  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName('V'),            'V');
  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName(ViaHeaderFull),  'ViaHeaderFull constant');
  CheckEquals(ViaHeaderFull, TIdSipHeader.CanonicaliseName(ViaHeaderShort), 'ViaHeaderShort constant');

  CheckEquals(WarningHeader, TIdSipHeader.CanonicaliseName('warning'),     'warning');
  CheckEquals(WarningHeader, TIdSipHeader.CanonicaliseName('Warning'),     'Warning');
  CheckEquals(WarningHeader, TIdSipHeader.CanonicaliseName(WarningHeader), 'WarningHeader constant');

  CheckEquals(WWWAuthenticateHeader, TIdSipHeader.CanonicaliseName('www-authenticate'),    'www-authenticate');
  CheckEquals(WWWAuthenticateHeader, TIdSipHeader.CanonicaliseName('WWW-Authenticate'),    'WWW-Authenticate');
  CheckEquals(WWWAuthenticateHeader, TIdSipHeader.CanonicaliseName(WWWAuthenticateHeader), 'WWWAuthenticateHeader constant');
end;

procedure TestTIdSipHeader.TestFullValue;
begin
  Self.H.Name := 'X-Foo';
  CheckEquals('', Self.H.FullValue, 'No value');

  Self.H.Value := 'bar';
  CheckEquals(Self.H.Value,
              Self.H.FullValue,
              'Simple value');

  Self.H.Value := ';bar';
  CheckEquals(Self.H.ParamsAsString,
              Self.H.FullValue,
              'No value, only parameters');

  Self.H.Value := 'bar;bar';
  CheckEquals(Self.H.Value + Self.H.ParamsAsString,
              Self.H.FullValue,
              'No value, only parameters');
end;

procedure TestTIdSipHeader.TestGetHeaderName;
begin
  CheckEquals('haha', TIdSipHeader.GetHeaderName('haha'),        'haha');
  CheckEquals('haha', TIdSipHeader.GetHeaderName('haha: kief'),  'haha: kief');
  CheckEquals('haha', TIdSipHeader.GetHeaderName('haha:kief'),   'haha:kief');
  CheckEquals('haha', TIdSipHeader.GetHeaderName('haha :kief'),  'haha :kief');
  CheckEquals('haha', TIdSipHeader.GetHeaderName('haha : kief'), 'haha : kief');
  CheckEquals('haha', TIdSipHeader.GetHeaderName(' haha'),       ' haha');
  CheckEquals('',     TIdSipHeader.GetHeaderName(''),            '''''');
  CheckEquals('',     TIdSipHeader.GetHeaderName(#0),            '#0');
end;

procedure TestTIdSipHeader.TestGetHeaderValue;
begin
  CheckEquals('',     TIdSipHeader.GetHeaderValue('haha'),        'haha');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue('haha: kief'),  'haha: kief');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue('haha:kief'),   'haha:kief');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue('haha :kief'),  'haha :kief');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue('haha : kief'), 'haha : kief');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue(' : kief'),     ' : kief');
  CheckEquals('kief', TIdSipHeader.GetHeaderValue(': kief'),      ': kief');
  CheckEquals('',     TIdSipHeader.GetHeaderValue(' haha'),       ' haha');
  CheckEquals('',     TIdSipHeader.GetHeaderValue(''),            '''''');
  CheckEquals('',     TIdSipHeader.GetHeaderValue(#0),            '#0');
end;

procedure TestTIdSipHeader.TestGetSetParam;
begin
  CheckEquals('', Self.H.Params['branch'], 'Value of non-existent param');

  Self.H.Params['branch'] := '';
  CheckEquals('', Self.H.Params['branch'], 'Value of param with empty string value');

  Self.H.Params['branch'] := 'f00';
  CheckEquals('f00', Self.H.Params['branch'], 'Value of param with non-empty string value');
end;

procedure TestTIdSipHeader.TestHasParam;
begin
  Check(not Self.H.HasParameter('foo'), 'New header, no params');

  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  Check(not Self.H.HasParameter('foo'), 'Some non-foo params');

  Self.H.Params['foo'] := 'bar';
  Check(Self.H.HasParameter('foo'), 'Some non-foo and foo params');
end;

procedure TestTIdSipHeader.TestEquals;
var
  Header: TIdSipHeader;
begin
  Self.H.Name := 'X-New-Header';
  Self.H.Value := 'secure;foo=bar';
  Header := TIdSipHeader.Create;
  try
    Header.Name  := Self.H.Name;
    Header.Value := Self.H.FullValue;
    Check(Self.H.Equals(Header), 'H = Header');
    Check(Header.Equals(Self.H), 'Header = H');

    Header.Name  := Self.H.Name;
    Header.Value := Uppercase(Self.H.FullValue);
    Check(Self.H.Equals(Header), 'H = Header, uppercase(value)');
    Check(Header.Equals(Self.H), 'Header = H, uppercase(value)');

    Header.Name  := 'X-Different-Header';
    Header.Value := Self.H.FullValue;
    Check(not Self.H.Equals(Header), 'H <> Header, name');
    Check(not Header.Equals(Self.H), 'Header <> H, name');

    Header.Name  := Self.H.Name;
    Header.Value := 'wombat' + Self.H.ParamsAsString;
    Check(not Self.H.Equals(Header), 'H <> Header, value');
    Check(not Header.Equals(Self.H), 'Header <> H, value');
  finally
    Header.Free;
  end;
end;

procedure TestTIdSipHeader.TestParamCount;
begin
  CheckEquals(0, Self.H.ParamCount, 'ParamCount of an empty list');

  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  CheckEquals(1, Self.H.ParamCount, 'ParamCount, 1 param');

  Self.H.Params['ttl'] := '5';
  CheckEquals(2, Self.H.ParamCount, 'ParamCount, 2 params');
end;

procedure TestTIdSipHeader.TestParamsAsString;
begin
  Self.H.Params['branch'] := 'z9hG4bK776asdhds';
  Self.H.Params['ttl']    := '5';
  Self.H.Params['foo']    := 'foo bar\';

  CheckEquals(';branch=z9hG4bK776asdhds;ttl=5;foo="foo bar\\"',
              Self.H.ParamsAsString,
              'ParamsAsString');
end;

procedure TestTIdSipHeader.TestRemoveAbsentParameter;
const
  Param = 'branch';
begin
  Self.H.RemoveParameter(Param);
  Check(not Self.H.HasParameter(Param),
        'Parameter somehow added during RemoveParameter');
end;

procedure TestTIdSipHeader.TestRemoveParameter;
const
  Param = 'branch';
begin
  Self.H.Params[Param] := 'z9hG4bK776asdhds';
  Self.H.RemoveParameter(Param);
  Check(not Self.H.HasParameter(Param),
        'Parameter not removed');
end;

procedure TestTIdSipHeader.TestValueParameterClearing;
begin
  Self.H.Value := 'Fighters;branch=haha';
  Self.H.Value := 'Fighters';
  CheckEquals('', Self.H.ParamsAsString, 'Parameters not cleared');
end;

procedure TestTIdSipHeader.TestValueWithNewParams;
begin
  Self.H.Value := 'Fighters;branch=haha';
  Self.H.Value := 'Fighters;tickle=feather';
  CheckEquals(';tickle=feather', Self.H.ParamsAsString, 'Parameters not cleared');
end;

procedure TestTIdSipHeader.TestValueWithQuotedParams;
begin
  Self.H.Value := 'Fighters;foo="haha"';
  CheckEquals(';foo=haha',
              Self.H.ParamsAsString,
              'Quotes not cleared');
end;

procedure TestTIdSipHeader.TestUnparsedValue;
var
  E: TIdSipExceptionRaisingHeader;
begin
  Self.H.Value := 'Fighters';
  CheckEquals(Self.H.Value,
              Self.H.UnparsedValue,
              'Normal value');

  E := TIdSipExceptionRaisingHeader.Create;
  try
    try
      E.Value := 'foo';
    except
      on EBadHeader do;
    end;

    CheckEquals('foo',
                E.UnparsedValue,
                'Invalid value');
  finally
    E.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipAddressHeader                                                    *
//******************************************************************************
//* TestTIdSipAddressHeader Public methods *************************************

procedure TestTIdSipAddressHeader.SetUp;
begin
  inherited SetUp;

  Self.A := Self.Header as TIdSipAddressHeader;
end;

//* TestTIdSipAddressHeader Protected methods **********************************

function TestTIdSipAddressHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipAddressHeader;
end;

//* TestTIdSipAddressHeader Published methods **********************************

procedure TestTIdSipAddressHeader.TestAsAddressOfRecord;
begin
  Self.A.Value := '"Hiro Protagonist Security Associates" <sip:hiro@enki.org>';
  CheckEquals(Self.A.AsAddressOfRecord,
              Self.A.Address.CanonicaliseAsAddressOfRecord,
              'AsAddressOfRecord');
end;

procedure TestTIdSipAddressHeader.TestAsString;
begin
  Self.A.Name := ToHeaderFull;

  Self.A.Value := 'sip:countzero@jacks-bar.com';
  CheckEquals(Self.A.Name + ': sip:countzero@jacks-bar.com',
              Self.A.AsString,
              'AsString, plain URI');

  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
  CheckEquals(Self.A.Name + ': Wintermute <sip:wintermute@tessier-ashpool.co.luna>',
              Self.A.AsString,
              'AsString, display-name');

  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>';
  CheckEquals(Self.A.Name + ': "Count Zero\"" <sip:countzero@jacks-bar.com>',
              Self.A.AsString,
              'AsString, display-name with quoted-pair');

  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>;paranoid';
  CheckEquals(Self.A.Name + ': "Count Zero\"" <sip:countzero@jacks-bar.com>;paranoid',
              Self.A.AsString,
              'AsString, display-name with quoted-pair + parameters');

  Self.A.Value := '"Count Zero" <sip:countzero@jacks-bar.com;paranoid>;very';
  CheckEquals(Self.A.Name + ': "Count Zero" <sip:countzero@jacks-bar.com;paranoid>;very',
              Self.A.AsString,
              'AsString, display-name, and URI and header have parameters');

  Self.A.Value         := '';
  Self.A.DisplayName   := 'Bell, Alexander';
  Self.A.Address.URI   := 'sip:a.g.bell@bell-tel.com';
  Self.A.Params['tag'] := '43';
  CheckEquals(Self.A.Name + ': "Bell, Alexander" <sip:a.g.bell@bell-tel.com>;tag=43',
              Self.A.AsString,
              'AsString, display-name with comma');
end;

procedure TestTIdSipAddressHeader.TestAsCanonicalAddress;
var
  URN: String;
begin
  URN := '<urn:uuid:' + ConstructUUID + '>';

  Self.A.Value := '"Count Zero" <sip:countzero:foo@jacks-bar.com;method=INVITE>;+sip.instance="' + URN + '>";expires=10';

  CheckEquals('"Count Zero" <sip:countzero@jacks-bar.com>;+sip.instance="' + URN + '>"',
              Self.A.AsCanonicalAddress,
              'AsCanonicalAddress');
end;

procedure TestTIdSipAddressHeader.TestAsToHeader;
var
  ToHeader: TIdSipToHeader;
begin
  Self.A.Address.Uri    := 'sip:countzero@jacks-bar.com;paranoid';
  Self.A.DisplayName    := 'Count Zero';
  Self.A.Params['very'] := '';

  ToHeader := Self.A.AsToHeader;
  try
    Check(Self.A.Address.Equals(ToHeader.Address), 'Address');
    CheckEquals(Self.A.DisplayName, ToHeader.DisplayName, 'Display name');
    CheckEquals(Self.A.ParamsAsString, ToHeader.ParamsAsString, 'Params');
  finally
    ToHeader.Free;
  end;
end;

procedure TestTIdSipAddressHeader.TestGrid;
const
  GridValue    = '1234';
  NewGridValue = '5678';
begin
  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna;grid=' + GridValue + '>';
  CheckEquals(GridValue, Self.A.Grid, 'Grid set by Value');

  Self.A.Grid := NewGridValue;
  CheckEquals(NewGridValue, Self.A.Grid, 'Grid property set');

  Self.A.Address.Grid := GridValue;
  CheckEquals(GridValue, Self.A.Grid, 'Grid property of Address set');
end;

procedure TestTIdSipAddressHeader.TestHasSipsUri;
begin
  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
  Check(not Self.A.HasSipsUri, 'SIP');

  Self.A.Address.URI := 'sips:wintermute@tessier-ashpool.co.luna';
  Check(Self.A.HasSipsUri, 'SIPS');
end;

procedure TestTIdSipAddressHeader.TestIsMalformed;
const
  FQDNLabelsCantStartWithDigits = 'sip:foo@1bar';
begin
  Self.A.Value := FQDNLabelsCantStartWithDigits;
  Check(Self.A.IsMalformed, 'Header not marked as malformed');
end;

procedure TestTIdSipAddressHeader.TestIsMalformedAfterSettingProperties;
const
  FQDNLabelsCantStartWithDigits = 'sip:foo@1bar';
begin
  Self.A.DisplayName := 'Foo Bar';
  Self.A.Address.Uri := FQDNLabelsCantStartWithDigits;

  Check(Self.A.IsMalformed,
        'Header thinks it''s well-formed, but the URI is malformed');
end;

procedure TestTIdSipAddressHeader.TestIsMalformedWithParsingAndManipulation;
const
  MalformedUri  = 'sip:127.0.0.1a';
  WellFormedUri = 'sip:127.0.0.1';
begin
  Self.A.Value := WellFormedUri;
  Check(not Self.A.IsMalformed, 'Malformed after setting Value');

  Self.A.Address.Scheme := 'sipp';
  Check(Self.A.IsMalformed, 'Well-formed after setting scheme');

  Self.A.Value := WellFormedUri;
  Check(not Self.A.IsMalformed, 'Malformed after resetting Value');

  Self.A.Address.Uri := MalformedUri;
  Check(Self.A.IsMalformed, 'Well-formed after setting address''s URI');

  Self.A.Address.Host := '127.0.0.1';
  Check(not Self.A.IsMalformed, 'Malformed after correcting host');
end;

procedure TestTIdSipAddressHeader.TestSetAddress;
var
  Addy: TIdSipAddressHeader;
begin
  Addy := TIdSipAddressHeader.Create;
  try
    Addy.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna';
    Self.A.Address := Addy.Address;

    CheckEquals(Addy.Address.URI, Self.A.Address.URI, 'SetAddress');
  finally
    Addy.Free;
  end;
end;

procedure TestTIdSipAddressHeader.TestValue;
begin
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                     Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueEmptyDisplayName;
begin
  A.Value := '<sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                     Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueEmptyString;
begin
  Self.A.Value := '';
  Check(Self.A.IsMalformed, 'Header not marked as malformed');
end;

procedure TestTIdSipAddressHeader.TestValueMissingScheme;
begin
  Self.A.Value := 'wintermute@tessier-ashpool.co.luna';
  Check(Self.A.IsMalformed,
        'Header not marked as malformed when URI has no scheme');
end;

procedure TestTIdSipAddressHeader.TestValueWithBlankQuotedName;
begin
  Self.A.Value := '"" <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',  Self.A.Address.URI,    'Address');
  CheckEquals('',                                        Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                        Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',  Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithEncodings;
begin
  Self.A.Value := '"Count Zero\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com',                  Self.A.Address.URI,    '1: Address');
  CheckEquals('Count Zero"',                                  Self.A.DisplayName,    '1: DisplayName');
  CheckEquals('',                                             Self.A.ParamsAsString, '1: Params');
  CheckEquals('"Count Zero\"" <sip:countzero@jacks-bar.com>', Self.A.Value,          '1: Value');

  Self.A.Value := '"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '2: Address');
  CheckEquals('Count\" Zero""',              Self.A.DisplayName,    '2: DisplayName');
  CheckEquals('',                            Self.A.ParamsAsString, '2: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '2: Value');

  Self.A.Value := '"\C\o\u\n\t\\\"\ \Z\e\r\o\"\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '3: Address');
  CheckEquals('Count\" Zero""',              Self.A.DisplayName,    '3: Name');
  CheckEquals('',                            Self.A.ParamsAsString, '3: Params');
  CheckEquals('"Count\\\" Zero\"\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '3: Value');

  Self.A.Value := '"Count Zero \\\\\\\"" <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com', Self.A.Address.URI,    '4: Address');
  CheckEquals('Count Zero \\\"',             Self.A.DisplayName,    '4: Name');
  CheckEquals('',                            Self.A.ParamsAsString, '4: Params');
  CheckEquals('"Count Zero \\\\\\\"" <sip:countzero@jacks-bar.com>',
              Self.A.Value,
              '4: Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithMalformedQuotedName;
begin
  // missing close quote
  Self.A.Value := '"Count Zero <sip:countzero@jacks-bar.com>';
  Check(Self.A.IsMalformed,
        'Failed to bail out because of unmatched quotes #1');

  // missing close quote
  Self.A.Value := '"Count Zero \" <sip:countzero@jacks-bar.com>';
  Check(Self.A.IsMalformed,
        'Failed to bail out because of unmatched quotes #2');

  // missing close quote
  Self.A.Value := '"Count Zero \\\\\\\" <sip:countzero@jacks-bar.com>';
  Check(Self.A.IsMalformed,
        'Failed to bail out because of unmatched quotes #3');
end;

procedure TestTIdSipAddressHeader.TestValueWithNormalName;
begin
  Self.A.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',              Self.A.Address.URI,    'Address');
  CheckEquals('Wintermute',                                        Self.A.DisplayName,    'DisplayName');
  CheckEquals('',                                                  Self.A.ParamsAsString, 'Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.luna>', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithNoWhitespaceBetweenDisplayNameAndUri;
begin
  Self.A.Value := '"caller"<sip:caller@example.com>';
  CheckEquals('sip:caller@example.com', Self.A.Address.URI,    'Address');
  CheckEquals('caller',                 Self.A.DisplayName,    'Name');
  CheckEquals('',                       Self.A.ParamsAsString, 'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithParam;
begin
  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna;hidden';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.A.Address.URI,    'Address');
  CheckEquals('',                                       Self.A.DisplayName,    'Name');
  CheckEquals(';hidden',                                Self.A.ParamsAsString, 'Params');
end;

procedure TestTIdSipAddressHeader.TestValueWithQuotedName;
begin
  Self.A.Value := '"Wintermute" <sip:wintermute@tessier-ashpool.co.luna>';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',              Self.A.Address.URI,    '1: Address');
  CheckEquals('Wintermute',                                          Self.A.DisplayName,    '1: Name');
  CheckEquals('',                                                    Self.A.ParamsAsString, '1: Params');
  CheckEquals('Wintermute <sip:wintermute@tessier-ashpool.co.luna>', Self.A.Value,          '1: Value');

  Self.A.Value := '"Count Zero" <sip:countzero@jacks-bar.com>';

  CheckEquals('sip:countzero@jacks-bar.com',                  Self.A.Address.URI,    '2: Address');
  CheckEquals('Count Zero',                                   Self.A.DisplayName,    '2: Name');
  CheckEquals('',                                             Self.A.ParamsAsString, '2: Params');
  CheckEquals('"Count Zero" <sip:countzero@jacks-bar.com>',   Self.A.Value,          '2: Value');

end;

procedure TestTIdSipAddressHeader.TestValueWithSpaceInDisplayName;
begin
  Self.A.Value := 'Count Zero <sip:countzero@jacks-bar.com>';
  CheckEquals('sip:countzero@jacks-bar.com',                Self.A.Address.URI,    'Address');
  CheckEquals('Count Zero',                                 Self.A.DisplayName,    'Name');
  CheckEquals('',                                           Self.A.ParamsAsString, 'Params');
  CheckEquals('"Count Zero" <sip:countzero@jacks-bar.com>', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithSpecialChars;
begin
  Self.A.Address.URI := 'sip:wintermute@tessier-ashpool.co.luna;tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna;tag=f00',   Self.A.Address.URI,    ';:Address');
  CheckEquals('',                                                 Self.A.DisplayName,    ';: Name');
  CheckEquals('',                                                 Self.A.ParamsAsString, ';: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.luna;tag=f00>', Self.A.Value,          ';: Value');

  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna?tag=f00';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna?tag=f00',   Self.A.Address.URI,    '?:Address');
  CheckEquals('',                                                 Self.A.DisplayName,    '?: Name');
  CheckEquals('',                                                 Self.A.ParamsAsString, '?: Params');
  CheckEquals('<sip:wintermute@tessier-ashpool.co.luna?tag=f00>', Self.A.Value,          '?: Value');

  Self.A.Value := 'sip:wintermute@tessier-ashpool.co.luna,tag=f00';
  Check(Self.A.IsMalformed, 'Header not marked as being malformed');
end;

procedure TestTIdSipAddressHeader.TestValueWithTrailingWhitespacePlusParam;
begin
  Self.A.Value := 'sip:vivekg@chair.dnrc.bell-labs.com ; haha=heehee';
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Address.URI,    'Address');
  CheckEquals('',                                    Self.A.DisplayName,    'DisplayName');
  CheckEquals(';haha=heehee',                        Self.A.ParamsAsString, 'Params');
  CheckEquals('sip:vivekg@chair.dnrc.bell-labs.com', Self.A.Value,          'Value');
end;

procedure TestTIdSipAddressHeader.TestValueWithUnquotedNonTokensPlusParam;
const
  DisplayName = 'Bell, Alexander';
  URI         = 'sip:a.g.bell@bell-tel.com';
begin
  Self.A.Value := DisplayName + ' <' + URI + '>';

  CheckEquals(DisplayName,
              Self.A.DisplayName,
              'display-name');
  CheckEquals(URI,
              Self.A.Address.URI,
              'URI');

  CheckEquals('"' + DisplayName + '" <' + URI + '>',
              Self.A.Value,
              'AsString');
end;

procedure TestTIdSipAddressHeader.TestValueWithQuotedURN;
const
  Address    = 'sip:wintermute@tessier-ashpool.co.luna.luna';
  InstanceID = '<urn:uuid:00000000-0000-0000-0000-000000000000>';
begin
  Self.A.Value := Address + ';+sip.instance="' + InstanceID + '"';

  CheckEquals(Address,
              Self.A.Address.AsString,
              'Address');
  CheckEquals(InstanceID,
              Self.A.Params[SipInstanceParam],
              '"' + SipInstanceParam + '" value');
end;

//******************************************************************************
//* TestTIdSipCommaSeparatedHeader                                             *
//******************************************************************************
//* TestTIdSipCommaSeparatedHeader Public methods ******************************

procedure TestTIdSipCommaSeparatedHeader.SetUp;
begin
  inherited SetUp;

  Self.C := Self.Header as TIdSipCommaSeparatedHeader;
end;

//* TestTIdSipCommaSeparatedHeader Protected methods ***************************

function TestTIdSipCommaSeparatedHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipCommaSeparatedHeader;
end;

//* TestTIdSipCommaSeparatedHeader Published methods ***************************

procedure TestTIdSipCommaSeparatedHeader.TestRemoveValues;
var
  Keepers: TStrings;
  I:       Integer;
  Remove:  TIdSipCommaSeparatedHeader;
begin
  Keepers := TStringList.Create;
  try
    Keepers.Add('Baz');
    Keepers.Add('Quaax');
    Keepers.Add('Qwglm');

    Remove := TIdSipCommaSeparatedHeader.Create;
    try
      Remove.Value := 'Foo, Bar';

      Self.C.Values.AddStrings(Keepers);
      Self.C.Values.AddStrings(Remove.Values);

      Self.C.RemoveValues(Remove);

      for I := 0 to Remove.Values.Count - 1 do
        CheckEquals(-1,
                    Self.C.Values.IndexOf(Remove.Values[I]),
                    '''' + Remove.Values[I] + ''' not removed');

      for I := 0 to Keepers.Count - 1 do
        CheckNotEquals(-1,
                       Self.C.Values.IndexOf(Keepers[I]),
                       '''' + Keepers[I] + ''' removed');
    finally
      Remove.Free;
    end;
  finally
    Keepers.Free;
  end;
end;

procedure TestTIdSipCommaSeparatedHeader.TestValue;
begin
  Self.C.Name := ContentLanguageHeader;

  Self.C.Value := '';
  CheckEquals(0, Self.C.Values.Count, 'Empty value');
  CheckEquals(Self.C.Name + ': ',
              Self.C.AsString,
              'Empty value AsString');

  Self.C.Value := 'a';
  CheckEquals(1, Self.C.Values.Count, 'a');
  CheckEquals('a', Self.C.Values[0], '1: First value');
  CheckEquals(Self.C.Name + ': a',
              Self.C.AsString,
              '''a'' AsString');

  Self.C.Value := 'a b';
  CheckEquals(1, Self.C.Values.Count, 'a b');
  CheckEquals('a b', Self.C.Values[0], '2: First value');
  CheckEquals(Self.C.Name + ': a b',
              Self.C.AsString,
              '''a b'' AsString');

  Self.C.Value := 'a,b';
  CheckEquals(2, Self.C.Values.Count, 'a,b');
  CheckEquals('a', Self.C.Values[0], '3: First value');
  CheckEquals('b', Self.C.Values[1], '3: Second value');
  CheckEquals(Self.C.Name + ': a, b',
              Self.C.AsString,
              '''a,b'' AsString');

  Self.C.Value := 'a, b';
  CheckEquals(2, Self.C.Values.Count, 'a, b');
  CheckEquals('a', Self.C.Values[0], '4: First value');
  CheckEquals('b', Self.C.Values[1], '4: Second value');
  CheckEquals(Self.C.Name + ': a, b',
              Self.C.AsString,
              '''a, b'' AsString');

  Self.C.Value := 'a;q=0.1, b;q=1';
  CheckEquals(2, Self.C.Values.Count, 'a;q=0.1, b;q=1');
  CheckEquals('a;q=0.1', Self.C.Values[0], '5: First value');
  CheckEquals('b;q=1', Self.C.Values[1],   '5: Second value');
  CheckEquals(Self.C.Name + ': a;q=0.1, b;q=1',
              Self.C.AsString,
              '''a;q=0.1, b;q=1'' AsString');
end;

//******************************************************************************
//* TestTIdSipAllowEventsHeader                                                 *
//******************************************************************************
//* TestTIdSipAllowEventsHeader Public methods **********************************

procedure TestTIdSipAllowEventsHeader.SetUp;
begin
  inherited SetUp;

  Self.AE := Self.Header as TIdSipAllowEventsHeader;
end;

//* TestTIdSipAllowEventsHeader Protected methods *******************************

function TestTIdSipAllowEventsHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipAllowEventsHeader;
end;

//* TestTIdSipAllowEventsHeader Published methods *******************************

procedure TestTIdSipAllowEventsHeader.TestIsMalformed;
begin
  // Too many dot-limited tokens in EventType name
  Self.AE.Value := 'foo.bar.baz';
  Check(Self.AE.IsMalformed, 'Header not marked as malformed');

  Self.AE.EventTypes[0] := 'foo.bar';
  Check(not Self.AE.IsMalformed, 'Header still marked as malformed');
end;

procedure TestTIdSipAllowEventsHeader.TestValue;
begin
  Self.AE.Value := 'foo.bar, foo.baz,foo.quaax';
  CheckEquals(3, Self.AE.EventTypeCount, 'EventTypeCount');
  CheckEquals('foo.bar',  Self.AE.EventTypes[0],  'EventTypes[0]');
  CheckEquals('foo.baz',  Self.AE.EventTypes[1],  'EventTypes[1]');
  CheckEquals('foo.quaax', Self.AE.EventTypes[2], 'EventTypes[2]');
end;

//******************************************************************************
//* TestTIdSipAuthorizationHeader                                              *
//******************************************************************************
//* TestTIdSipAuthorizationHeader Public methods *******************************

procedure TestTIdSipAuthorizationHeader.SetUp;
begin
  inherited SetUp;

  Self.A := Self.Header as TIdSipAuthorizationHeader;
end;

//* TestTIdSipAuthorizationHeader Protected methods ****************************

function TestTIdSipAuthorizationHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipAuthorizationHeader;
end;

//* TestTIdSipAuthorizationHeader Published methods ****************************

procedure TestTIdSipAuthorizationHeader.TestAlgorithm;
var
  Value: String;
begin
  Value := 'SHA-1024';
  Self.A.Algorithm := Value;
  CheckEquals(Value,
              Self.A.Algorithm,
              'Algorithm');
end;

procedure TestTIdSipAuthorizationHeader.TestCNonce;
var
  Value: String;
begin
  Value := '00f00f00f00f';
  Self.A.CNonce := Value;
  CheckEquals(Value,
              Self.A.CNonce,
              'CNonce');
end;

procedure TestTIdSipAuthorizationHeader.TestDigestResponse;
var
  Value: String;
begin
  Value := 'f00f00f00f00';
  Self.A.Response := Value;
  CheckEquals(Value,
              Self.A.Response,
              'DigestResponse');
end;

procedure TestTIdSipAuthorizationHeader.TestDigestUri;
var
  Value: String;
begin
  Value := 'sip:sos@tessier-ashpool.co.luna';
  Self.A.DigestUri := Value;
  CheckEquals(Value,
              Self.A.DigestUri,
              'DigestUri');
end;

procedure TestTIdSipAuthorizationHeader.TestGetValue;
begin
  Self.A.AuthorizationScheme := 'foo';
  Self.A.Nonce := 'aefbb';
  Self.A.NonceCount := $f00f;
  Self.A.Algorithm := 'sha1-512';
  Self.A.Qop := QopAuth;
  Self.A.Realm := 'tessier-ashpool.co.luna';
  Self.A.Username := 'Wintermute';
  Self.A.UnknownResponses['paranoid'] := '\very';

  CheckEquals('foo nonce="aefbb",'
            + 'nc=0000f00f,'
            + 'algorithm="sha1-512",'
            + 'qop=auth,'
            + 'realm="tessier-ashpool.co.luna",'
            + 'username="Wintermute",'
            + 'paranoid="\\very"',
              Self.A.Value,
              'Value');
end;

procedure TestTIdSipAuthorizationHeader.TestHasParam;
const
  UnknownParam = 'foo';
begin
  Self.A.Nonce := 'foofoo';

  Check(Self.A.HasParameter(NonceParam),
        'Header thinks it doesn''t have a ' + NonceParam + ' parameter');

  Self.A.UnknownResponses[UnknownParam] := 'foo';
  Check(Self.A.HasParameter(NonceParam),
        'Header thinks it doesn''t have a ' + UnknownParam + ' parameter');
end;

procedure TestTIdSipAuthorizationHeader.TestIsBasic;
begin
  Self.A.AuthorizationScheme := 'foo';
  Check(not Self.A.IsBasic, 'foo');

  Self.A.AuthorizationScheme := Lowercase(BasicAuthorizationScheme);
  Check(Self.A.IsBasic, Lowercase(BasicAuthorizationScheme));

  Self.A.AuthorizationScheme := BasicAuthorizationScheme;
  Check(Self.A.IsBasic, BasicAuthorizationScheme);
end;

procedure TestTIdSipAuthorizationHeader.TestIsDigest;
begin
  Self.A.AuthorizationScheme := 'foo';
  Check(not Self.A.IsDigest, 'foo');

  Self.A.AuthorizationScheme := Lowercase(DigestAuthorizationScheme);
  Check(Self.A.IsDigest, Lowercase(DigestAuthorizationScheme));

  Self.A.AuthorizationScheme := DigestAuthorizationScheme;
  Check(Self.A.IsDigest, DigestAuthorizationScheme);
end;


procedure TestTIdSipAuthorizationHeader.TestIsNonce;
begin
  Check(Self.A.IsNonce(''), '''''');
  Check(Self.A.IsNonce('foo'), 'foo');
  Check(Self.A.IsNonce('fo\"o'), 'fo\"o');
  Check(not Self.A.IsNonce('foo\'), 'foo\');
  Check(Self.A.IsNonce('foo\\'), 'foo\\');
end;

procedure TestTIdSipAuthorizationHeader.TestName;
begin
  CheckEquals(AuthorizationHeader, Self.A.Name, 'Name');
end;

procedure TestTIdSipAuthorizationHeader.TestNC;
begin
  Self.A.NonceCount := $f00f;
  CheckEquals('0000f00f',
              Self.A.NC,
              'f00f');

  Self.A.NonceCount := $decafbad;
  CheckEquals('decafbad',
              Self.A.NC,
              'decafbad');

  Self.A.NonceCount := $0;
  CheckEquals('00000000',
              Self.A.NC,
              '0');
end;

procedure TestTIdSipAuthorizationHeader.TestNonce;
var
  Value: String;
begin
  Value := 'f00f00f00f00';
  Self.A.Nonce := Value;
  CheckEquals(Value,
              Self.A.Nonce,
              'Nonce');
end;

procedure TestTIdSipAuthorizationHeader.TestNonceCount;
begin
  Self.A.NonceCount := $f00f;
  CheckEquals(IntToHex($f00f, 4),
              IntToHex(Self.A.NonceCount, 4),
              '$f00f');

  Self.A.NonceCount := 0;
  CheckEquals(IntToHex(0, 4),
              IntToHex(Self.A.NonceCount, 4),
              '0');
end;

procedure TestTIdSipAuthorizationHeader.TestOpaque;
var
  Value: String;
begin
  Value := 'transparent';
  Self.A.Opaque := Value;
  CheckEquals(Value,
              Self.A.Opaque,
              'Opaque');
end;

procedure TestTIdSipAuthorizationHeader.TestQop;
var
  Value: String;
begin
  Value := 'don''t even know what this is';
  Self.A.Qop := Value;
  CheckEquals(Value,
              Self.A.Qop,
              'Qop');
end;

procedure TestTIdSipAuthorizationHeader.TestRealm;
var
  Value: String;
begin
  Value := 'tessier-ashpool.co.luna';
  Self.A.Realm := Value;
  CheckEquals(Value,
              Self.A.Realm,
              'Realm');
end;

procedure TestTIdSipAuthorizationHeader.TestUnknownResponses;
var
  Value: String;
begin
  Value := 'Wonky';
  Self.A.UnknownResponses['skew'] := Value;
  CheckEquals(Value,
              Self.A.UnknownResponses['skew'],
              'Unknown response');

  Value := '';
  Self.A.UnknownResponses['skew'] := Value;
  CheckEquals(Value,
              Self.A.UnknownResponses['skew'],
              'Unknown response, blanked out');
end;

procedure TestTIdSipAuthorizationHeader.TestUnquotedResponse;
begin
  Self.A.Value := 'Digest username=Alice"';
  Check(Self.A.IsMalformed,
        'Failed to bail out on quoted-string without leading quote');

  Self.A.Value := 'Digest username="Alice';
  Check(Self.A.IsMalformed,
        'Failed to bail out on quoted-string without trailing quote');
end;

procedure TestTIdSipAuthorizationHeader.TestUsername;
var
  Value: String;
begin
  Value := 'Alice';
  Self.A.Username := Value;
  CheckEquals(Value,
              Self.A.Username,
              'Username');
end;

procedure TestTIdSipAuthorizationHeader.TestValue;
begin
  Self.A.Value := 'Digest username="Alice",realm="atlanta.com", '
                + 'algorithm="MD5", '
                + 'cnonce="f00f00", '
                + 'nonce="84a4cc6f3082121f32b42a2187831a9e", '
                + 'nc=8f, '
                + 'opaque="aaaabbbb", '
                + 'otherparam=foo,'
                + 'qop=token,'
                + 'uri="tel://112", '
                + 'response="7587245234b3434cc3412213e5f113a5432"';

  Check(Self.A.IsDigest, 'Authorization Scheme');
  CheckEquals('MD5',
              Self.A.Algorithm,
              'Algorithm');
  CheckEquals('f00f00',
              Self.A.CNonce,
              'CNonce');
  CheckEquals('7587245234b3434cc3412213e5f113a5432',
              Self.A.Response,
              'DigestResponse');
  CheckEquals('tel://112',
              Self.A.DigestUri,
              'DigestUri');
  CheckEquals('84a4cc6f3082121f32b42a2187831a9e',
              Self.A.Nonce,
              'Nonce');
  CheckEquals(IntToHex($8f, 2),
              IntToHex(Self.A.NonceCount, 2),
              'NonceCount');
  CheckEquals('aaaabbbb',
              Self.A.Opaque,
              'Opaque');
  CheckEquals('token',
              Self.A.Qop,
              'Qop');
  CheckEquals('atlanta.com',
              Self.A.Realm,
              'Realm');
  CheckEquals('Alice',
              Self.A.Username,
              'Username');
  CheckEquals('foo',
              Self.A.UnknownResponses['otherparam'],
              'otherparam');
end;

procedure TestTIdSipAuthorizationHeader.TestValueQuotedNonceCount;
begin
  // RFC 2617 section 3.2.2 tells us that we mustn't quote the nc param.
  // However, it seems likely that at least some servers will quote the value.
  // And it causes no harm for us to accept these quoted values.

  Self.A.Value := 'Digest nc="0000f00f"';

  CheckEquals(IntToHex($f00f, 8),
              IntToHex(Self.A.NonceCount, 8),
              'Quoted NonceCount param');
end;

procedure TestTIdSipAuthorizationHeader.TestValueQuotedQop;
begin
  // RFC 2617 section 3.2.1 tells us that we mustn't quote the qop param.
  // However, it seems likely that at least some servers will quote the value.
  // And it causes no harm for us to accept these quoted values.

  Self.A.Value := 'Digest qop="' + QopAuth + '"';

  CheckEquals(QopAuth,
              Self.A.Qop,
              'Quoted qop param');
end;

procedure TestTIdSipAuthorizationHeader.TestValueSingleParam;
begin
  Self.A.Value := 'Digest username="Alice"';

  CheckEquals('Alice', Self.A.Username, 'Username');
end;

//******************************************************************************
//* TestTIdSipCallIDHeader                                                     *
//******************************************************************************
//* TestTIdSipCallIDHeader Public methods **************************************

procedure TestTIdSipCallIDHeader.SetUp;
begin
  inherited SetUp;

  C := Self.Header as TIdSipCallIDHeader;
end;

//* TestTIdSipCallIDHeader Protected methods ***********************************

function TestTIdSipCallIDHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipCallIDHeader;
end;

//* TestTIdSipCallIDHeader Published methods ***********************************

procedure TestTIdSipCallIDHeader.TestEquals;
var
  CallID: TIdSipCallIDHeader;
  H:      TIdSipHeader;
begin
  CallID := TIdSipCallIDHeader.Create;
  try
    Self.C.Value := 'fdjhasdfa';

    CallID.Value := Self.C.Value;
    Check(Self.C.Equals(CallID), 'C = CallID');
    Check(CallID.Equals(Self.C), 'CallID = C');

    CallID.Value := Uppercase(Self.C.Value);
    Check(not Self.C.Equals(CallID), 'C <> CallID, case-sensitive');
    Check(not CallID.Equals(Self.C), 'CallID <> C, case-sensitive');

    CallID.Value := Self.C.Value + Self.C.Value;
    Check(not Self.C.Equals(CallID), 'C <> CallID, different value');
    Check(not CallID.Equals(Self.C), 'CallID <> C, different value');
  finally
    CallID.Free;
  end;

  H := TIdSipHeader.Create;
  try
    H.Name := Self.C.Name;
    H.Value := Self.C.Value;

    Check(H.Equals(Self.C), 'H = C');
    Check(H.Equals(Self.C), 'C = H');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipCallIDHeader.TestValue;
begin
  Self.C.Value := 'fdjhasdfa';
  CheckEquals('fdjhasdfa', Self.C.Value, 'fdjhasdfa');
  Self.C.Value := 'fdjhasdfa@sda';
  CheckEquals('fdjhasdfa@sda', Self.C.Value, 'fdjhasdfa@sda');

  Self.C.Value := '';
  Check(Self.C.IsMalformed,
        'Failed to bail out on empty string');

  Self.C.Value := 'aaaaaaaaaaaaaaaa;';
  Check(Self.C.IsMalformed,
       'Failed to bail out on non-word');

  Self.C.Value := 'aaaaaaaa@@bbbbb';
  Check(Self.C.IsMalformed,
        'Failed to bail out optional non-word');
end;

procedure TestTIdSipCallIDHeader.TestValueWithParams;
begin
  Self.C.Value := 'one@two;tag=f00';
  Check(Self.C.IsMalformed,
        'Failed to bail out with params - semicolon is an invalid character');
end;

//******************************************************************************
//* TestTIdSipContactHeader                                                    *
//******************************************************************************
//* TestTIdSipContactHeader Public methods *************************************

procedure TestTIdSipContactHeader.SetUp;
begin
  inherited SetUp;

  Self.C := Self.Header as TIdSipContactHeader;
end;

//* TestTIdSipContactHeader Protected methods **********************************

function TestTIdSipContactHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipContactHeader;
end;

//* TestTIdSipContactHeader Published methods **********************************

procedure TestTIdSipContactHeader.TestAssign;
var
  Other: TIdSipContactHeader;
begin
  Other := TIdSipContactHeader.Create;
  try
    Self.C.IsUnset := true;

    Other.Assign(Self.C);
    Check(Other.IsUnset, 'IsUnset not copied');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipContactHeader.TestIsContact;
begin
  Check(Self.C.IsContact,
        Self.C.ClassName + ' doesn''t think it''s a Contact header');
end;

procedure TestTIdSipContactHeader.TestIsMalformedWildCardUri;
begin
  Self.C.IsWildCard := true;

  Check(not Self.C.IsMalformed,
        'The wild card "URI", while unusual, is still a valid Contact header');
end;

procedure TestTIdSipContactHeader.TestName;
begin
  CheckEquals(ContactHeaderFull, Self.C.Name, 'Name');

  Self.C.Name := 'foo';
  CheckEquals(ContactHeaderFull, Self.C.Name, 'Name after set');
end;

procedure TestTIdSipContactHeader.TestGetSetExpires;
begin
  Self.C.Expires := 0;
  CheckEquals(0, Self.C.Expires, '0');

  Self.C.Expires := 666;
  CheckEquals(666, Self.C.Expires, '666');
end;

procedure TestTIdSipContactHeader.TestGetSetQ;
begin
  Self.C.Q := 0;
  CheckEquals(0, Self.C.Q, '0');

  Self.C.Q := 666;
  CheckEquals(666, Self.C.Q, '666');
end;

procedure TestTIdSipContactHeader.TestGetValueWithStar;
begin
  Self.C.Value := ContactWildCard;
  CheckEquals(ContactWildCard, Self.C.Value, 'Value with star');
end;

procedure TestTIdSipContactHeader.TestGruu;
const
  FirstGruu  = 'sip:wintermute@tessier-ashpool.co.luna;opaque=foo';
  SecondGruu = 'sip:wintermute@tessier-ashpool.co.luna;opaque=bar';
  ThirdGruu  = 'sip:wintermute@tessier-ashpool.co.luna;opaque=baz';
begin
  Self.C.Gruu := FirstGruu;
  CheckEquals(FirstGruu,
              Self.C.Gruu,
              FirstGruu);
  CheckEquals(Self.C.Address.AsString + ';gruu="' + Self.C.Gruu + '"',
              Self.C.FullValue,
              'gruu param must be quoted');

  Self.C.Gruu := SecondGruu;
  CheckEquals(SecondGruu,
              Self.C.Gruu,
              SecondGruu);

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;gruu="' + ThirdGruu + '"';
  CheckEquals(ThirdGruu,
              Self.C.Gruu,
              ThirdGruu);
end;

procedure TestTIdSipContactHeader.TestRemoveExpires;
begin
  Self.C.RemoveExpires;
  Check(not Self.C.WillExpire, 'No expires param');

  Self.C.Expires := 0;
  Self.C.RemoveExpires;
  Check(not Self.C.WillExpire, 'Expires param');
end;

procedure TestTIdSipContactHeader.TestValue;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Self.C.Address.Uri,
              'Uri');

  Self.C.Value := 'Wintermute <sip:wintermute@tessier-ashpool.co.luna>';
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
              Self.C.Address.Uri,
              'Uri: Display name + Uri');
  CheckEquals('Wintermute',
              Self.C.DisplayName,
              'Display name: Display name + Uri');

  Self.C.Value := '"Hiro Protagonist" <sip:hiro@enki.org>';
  CheckEquals('sip:hiro@enki.org',
              Self.C.Address.Uri,
              'Uri: Display name with spaces + Uri');
  CheckEquals('Hiro Protagonist',
              Self.C.DisplayName,
              'Display name: Display name with spaces + Uri');
end;

procedure TestTIdSipContactHeader.TestValueWithExpires;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=0';
  CheckEquals(0, C.Expires, 'expires=0');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=666';
  CheckEquals(666, C.Expires, 'expires=666');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=65536';
  CheckEquals(65536, C.Expires, 'expires=65536');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=a';
  Check(Self.C.IsMalformed,
        'Failed to bail out with letters');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=-1';
  Check(Self.C.IsMalformed,
        'Failed to bail out with negative number');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;expires=';
  Check(Self.C.IsMalformed,
        'Failed to bail out with empty string');
end;

procedure TestTIdSipContactHeader.TestValueInstanceID;
const
  ZeroURN = '<urn:uuid:00000000-0000-0000-0000-000000000000>';
begin
  Self.C.Value := '<sip:wintermute@tessier-ashpool.co.luna>;+sip.instance="<malformed-urn>"';
  Check(Self.C.IsMalformed,
        'Contact header not marked as malformed with an invalid sip.instance parameter');

  Self.C.Value := '<sip:wintermute@tessier-ashpool.co.luna>;+sip.instance="' + ZeroURN + '"';
  CheckEquals(ZeroURN,
              Self.C.SipInstance,
              'SipInstance');
end;

procedure TestTIdSipContactHeader.TestValueWithQ;
begin
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna';

  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.C.Address.URI,    'Address');
  CheckEquals('',                                     Self.C.DisplayName,    'DisplayName');
  CheckEquals('',                                     Self.C.ParamsAsString, 'Params');
  CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.C.Value,          'Value');
  Check(                                              not Self.C.IsWildCard, 'IsWild');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0';
  CheckEquals(0, C.Q, 'q=0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.0';
  CheckEquals(0, C.Q, 'q=0.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.00';
  CheckEquals(0, C.Q, 'q=0.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.000';
  CheckEquals(0, C.Q, 'q=0.000');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.123';
  CheckEquals(123, C.Q, 'q=0.123');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.666';
  CheckEquals(666, C.Q, 'q=0.666');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1';
  CheckEquals(1000, C.Q, 'q=1');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.0';
  CheckEquals(1000, C.Q, 'q=1.0');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.00';
  CheckEquals(1000, C.Q, 'q=1.00');
  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.000';
  CheckEquals(1000, C.Q, 'q=1.000');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=';
  Check(Self.C.IsMalformed,
        'Failed to bail out on empty string');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=a';
  Check(Self.C.IsMalformed,
        'Failed to bail out on letters');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=0.1234';
  Check(Self.C.IsMalformed,
        'Failed to bail out on too many digits');

  Self.C.Value := 'sip:wintermute@tessier-ashpool.co.luna;q=1.1';
  Check(Self.C.IsMalformed,
        'Failed to bail out on number too big');
end;

procedure TestTIdSipContactHeader.TestValueWithParams;
const
  Address    = 'sip:wintermute@tessier-ashpool.co.luna';
  ParamName  = 'foo';
  ParamValue = 'bar';
begin
  Self.C.Value := Address + ';' + ParamName + '=' + ParamValue;

  CheckEquals(Address, Self.C.Address.AsString, 'Address');
  Check(Self.C.HasParameter(ParamName),
        'Param must belong to the HEADER, not the URI');
  CheckEquals(ParamValue,
              Self.C.Params[ParamName],
              'Value of "' + ParamName + '"');
end;

procedure TestTIdSipContactHeader.TestValueWithStar;
const
  ExpireAllWithQ = '*;q=0.1';
begin
  Self.C.Value := '*';
  Check(Self.C.IsWildCard, '*');

  Self.C.Value := ContactWildCard;
  Check(Self.C.IsWildCard, 'ContactWildCard');

  Self.C.Value := ExpireAllWithQ;
  Check(Self.C.IsWildCard, ExpireAllWithQ);
  CheckEquals(100, Self.C.Q, 'QValue: ' + ExpireAllWithQ);
  CheckEquals(ExpireAllWithQ,
              Self.C.FullValue,
              'Wildcard + parameters not parsed correctly');
end;

procedure TestTIdSipContactHeader.TestWillExpire;
begin
  Check(not Self.C.WillExpire, 'No expire param');
  Self.C.Expires := 2;
  Check(Self.C.WillExpire, 'Expire param set via property');

  Self.C.Params[ExpiresParam] := '';
  Self.C.Params[ExpiresParam] := '2';
  Check(Self.C.WillExpire, 'Expire param set directly');
end;

//******************************************************************************
//* TestTIdSipContentDispositionHeader                                         *
//******************************************************************************
//* TestTIdSipContentDispositionHeader Public methods **************************

procedure TestTIdSipContentDispositionHeader.SetUp;
begin
  inherited SetUp;

  Self.C := Self.Header as TIdSipContentDispositionHeader;
end;

//* TestTIdSipContentDispositionHeader Protected methods ***********************

function TestTIdSipContentDispositionHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipContentDispositionHeader;
end;

//* TestTIdSipContentDispositionHeader Published methods ***********************

procedure TestTIdSipContentDispositionHeader.TestGetSetHandling;
begin
  Self.C.Handling := HandlingRequired;
  CheckEquals(HandlingRequired, Self.C.Handling, HandlingRequired);

  Self.C.Handling := HandlingOptional;
  CheckEquals(HandlingOptional, Self.C.Handling, HandlingOptional);
end;

procedure TestTIdSipContentDispositionHeader.TestIsSession;
begin
  Self.C.Value := '';
  Check(not Self.C.IsSession, 'Empty string');

  Self.C.Value := DispositionRender;
  Check(not Self.C.IsSession, DispositionRender);

  Self.C.Value := DispositionSession;
  Check(Self.C.IsSession, DispositionSession);

  Self.C.Value := UpperCase(DispositionSession);
  Check(Self.C.IsSession, UpperCase(DispositionSession));
end;

procedure TestTIdSipContentDispositionHeader.TestName;
begin
  CheckEquals(ContentDispositionHeader, Self.C.Name, 'Name');

  Self.C.Name := 'foo';
  CheckEquals(ContentDispositionHeader, Self.C.Name, 'Name after set');
end;

procedure TestTIdSipContentDispositionHeader.TestValue;
begin
  Self.C.Value := 'foo';
  CheckEquals('foo', Self.C.Value, 'Handling foo');
  Self.C.Value := DispositionSession;
  CheckEquals(DispositionSession, Self.C.Value, 'Handling foo');
end;

//******************************************************************************
//* TestTIdSipCSeqHeader                                                       *
//******************************************************************************
//* TestTIdSipCSeqHeader Public methods ****************************************

procedure TestTIdSipCSeqHeader.SetUp;
begin
  inherited SetUp;

  Self.C := Self.Header as TIdSipCSeqHeader;
end;

//* TestTIdSipCSeqHeader Published methods *************************************

function TestTIdSipCSeqHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipCSeqHeader;
end;

//* TestTIdSipCSeqHeader Published methods *************************************

procedure TestTIdSipCSeqHeader.TestIncrement;
var
  I: Integer;
begin
  Self.C.SequenceNo := 1;
  for I := Self.C.SequenceNo + 1 to Self.C.SequenceNo + 10 do begin
    Self.C.Increment;
    CheckEquals(I,
                Self.C.SequenceNo,
                'SequenceNo not incremented, I = ' + IntToStr(I));
  end;

  Self.C.SequenceNo := High(Self.C.SequenceNo);
  Self.C.Increment;
  CheckEquals(0, Self.C.SequenceNo, 'SequenceNo rollover');
end;

procedure TestTIdSipCSeqHeader.TestValue;
begin
  Self.C.Value := '1 INVITE';
  CheckEquals(1,        Self.C.SequenceNo, 'SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     'Method');

  Self.C.Value := '1  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '2: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '2: Method');

  Self.C.Value := '1'#13#10'  INVITE';
  CheckEquals(1,        Self.C.SequenceNo, '3: SequenceNo');
  CheckEquals('INVITE', Self.C.Method,     '3: Method');

  Self.C.Value := 'a';
  Check(Self.C.IsMalformed,
        'Failed to bail out with a non-numeric sequence number, ''a''');

  Self.C.Value := 'cafebabe INVITE';
  Check(Self.C.IsMalformed,
        'Failed to bail out with a non-numeric sequence number, ''cafebabe INVITE''');

  Self.C.Value := '42 ';
  Check(Self.C.IsMalformed,
        'Failed to bail out with a non-method, ''42 ''');

  Self.C.Value := '42 "INVITE"';
  Check(Self.C.IsMalformed,
        'Failed to bail out with a non-method, ''42 "INVITE"');
end;

procedure TestTIdSipCSeqHeader.TestVeryLargeLegalValue;
begin
  Self.C.Value := '3735928559 BYE'; // $deadbeef in base-10
  Check(not Self.C.IsMalformed,
        'CSeq must support a sequence number N > (2^32)-1');
end;

procedure TestTIdSipCSeqHeader.TestVeryLargeValue;
begin
  Self.C.Value := '4294967297 INVITE';
  Check(Self.C.IsMalformed,
        'Failed to bail out with a ridiculously large CSeq sequence number');
end;

//******************************************************************************
//* TestTIdSipDateHeader                                                       *
//******************************************************************************
//* TestTIdSipDateHeader Public methods ****************************************

procedure TestTIdSipDateHeader.SetUp;
begin
  inherited SetUp;

  Self.D := Self.Header as TIdSipDateHeader;
end;

//* TestTIdSipDateHeader Protected methods *************************************

function TestTIdSipDateHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipDateHeader;
end;

//* TestTIdSipDateHeader Published methods *************************************

procedure TestTIdSipDateHeader.TestName;
begin
  CheckEquals(DateHeader, Self.D.Name, 'Name');

  Self.D.Name := 'foo';
  CheckEquals(DateHeader, Self.D.Name, 'Name after set');
end;

procedure TestTIdSipDateHeader.TestGetValue;
var
  DT: TDateTime;
begin
  DT := EncodeDate(1990, 12, 13) + EncodeTime(10, 22, 33, 44);
  Self.D.Time.SetFromTDateTime(DT);
  CheckEquals('Thu, 13 Dec 1990 10:22:33 +0000',
              Self.D.Value,
              'Value must derive from the Time property');
end;

procedure TestTIdSipDateHeader.TestValue;
begin
  Self.D.Value := 'Fri, 18 Jul 2003 16:00:00 GMT';

  CheckEquals('2003/07/18 16:00:00',
              FormatDateTime('yyyy/mm/dd hh:mm:ss', Self.D.Time.AsTDateTime),
              'AbsoluteTime');
end;

procedure TestTIdSipDateHeader.TestValueMalformedAbsoluteTime;
begin
  Self.D.Value := 'Thu, 44 Dec 19999 16:00:00 EDT';
  Check(Self.D.IsMalformed,
        'Failed to bail out');
end;

procedure TestTIdSipDateHeader.TestValueRelativeTime;
begin
  Self.D.Value := '1';
  Check(Self.D.IsMalformed,
        'Failed to bail out');
end;

procedure TestTIdSipDateHeader.TestValueZeroTime;
begin
  // A degenerate case.
  Self.D.Value := 'Wed, 30 Dec 1899 00:00:00 GMT';
  CheckEquals(0, Self.D.Time.AsTDateTime, 'Zero Time');
end;

//******************************************************************************
//* TestTIdSipEventHeader                                                      *
//******************************************************************************
//* TestTIdSipEventHeader Public methods ***************************************

procedure TestTIdSipEventHeader.SetUp;
begin
  inherited SetUp;

  Self.E := Self.Header as TIdSipEventHeader;
end;

function TestTIdSipEventHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipEventHeader;
end;

procedure TestTIdSipEventHeader.TestEquals;
var
  Other: TIdSipEventHeader;
begin
  Other := TIdSipEventHeader.Create;
  try
    Self.E.Value := 'foo';
    Other.Value := Self.E.Value;
    Check(Self.E.Equals(Other), 'E <> Other');
    Check(Other.Equals(Self.E), 'Other <> E');

    Other.Value := 'bar';
    Check(not Self.E.Equals(Other), 'E = Other');
    Check(not Other.Equals(Self.E), 'Other = E');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipEventHeader.TestEqualsWithID;
var
  Other: TIdSipEventHeader;
begin
  Other := TIdSipEventHeader.Create;
  try
    Self.E.Value := 'foo;id=bar';
    Other.Value := Self.E.FullValue;

    Check(Self.E.Equals(Other), 'E <> Other');
    Check(Other.Equals(Self.E), 'Other <> E');

    Other.ID := 'baz';
    Check(not Self.E.Equals(Other), 'E = Other');
    Check(not Other.Equals(Self.E), 'Other = E');

    Other.Params['unused'] := '1';
    Check(not Self.E.Equals(Other), 'E = Other: non-id param not ignored');
    Check(not Other.Equals(Self.E), 'Other = E: non-id param not ignored');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipEventHeader.TestEqualsWithNonIDParams;
var
  Other: TIdSipEventHeader;
begin
  Other := TIdSipEventHeader.Create;
  try
    Self.E.Value := 'foo;id=bar';
    Other.Value := Self.E.FullValue;
    Self.E.Params['baz'] := '2';

    Check(Self.E.Equals(Other), 'E <> Other: non-id param not ignored');
    Check(Other.Equals(Self.E), 'Other <> E: non-id param not ignored');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipEventHeader.TestGetSetID;
begin
  Self.E.ID := 'foo';
  CheckEquals('foo', Self.E.ID, 'First set/get');

  Self.E.ID := 'bar';
  CheckEquals('bar', Self.E.ID, 'Second set/get');

  Self.E.ID := '';
  Check(not Self.E.HasParameter(IdParam),
        'id param not removed when set to the empty string');
end;

procedure TestTIdSipEventHeader.TestIsEventType;
begin
  Check(not TIdSipEventHeader.IsEventType(''),            'Empty string');
  Check(not TIdSipEventHeader.IsEventType('.'),           '.');
  Check(not TIdSipEventHeader.IsEventType('token.'),      'token.');
  Check(not TIdSipEventHeader.IsEventType('.token'),      '.token');
  Check(    TIdSipEventHeader.IsEventType('token'),       'token');
  Check(    TIdSipEventHeader.IsEventType('token.token'), 'token.token');
end;

procedure TestTIdSipEventHeader.TestIsTokenNoDot;
begin
  Check(not TIdSipEventHeader.IsTokenNoDot(''),            'Empty string');
  Check(not TIdSipEventHeader.IsTokenNoDot('.'),           '.');
  Check(    TIdSipEventHeader.IsTokenNoDot('token-nodot'), 'token-nodot');
  Check(not TIdSipEventHeader.IsTokenNoDot('token.dot'),   'token.dot');
end;

procedure TestTIdSipEventHeader.TestMalformedValue;
begin
  // Too many dot-limited tokens
  Self.E.Value := 'foo.bar.baz';
  Check(Self.E.IsMalformed, 'Header not marked as malformed');
end;

procedure TestTIdSipEventHeader.TestValue;
begin
  Self.E.Value := 'foo';

  CheckEquals('foo', Self.E.Value,         'foo: Value');
  CheckEquals('foo', Self.E.EventType,     'foo: EventType');
  CheckEquals('foo', Self.E.EventPackage,  'foo: EventPackage');
  CheckEquals('',    Self.E.EventTemplate, 'foo: EventTemplate');

  Self.E.Value := 'foo.bar';
  CheckEquals('foo.bar', Self.E.Value,         'foo.bar: Value');
  CheckEquals('foo.bar', Self.E.EventType,     'foo.bar: EventType');
  CheckEquals('foo',     Self.E.EventPackage,  'foo.bar: EventPackage');
  CheckEquals('bar',     Self.E.EventTemplate, 'foo.bar: EventTemplate');
end;

procedure TestTIdSipEventHeader.TestValueWithID;
begin
  Self.E.Value := 'foo;id=bar';

  CheckEquals('bar', Self.E.ID, 'ID not set');
end;

//******************************************************************************
//* TestTIdSipFromToHeader                                                     *
//******************************************************************************
//* TestTIdSipFromToHeader Public methods **************************************

procedure TestTIdSipFromToHeader.SetUp;
begin
  inherited SetUp;

  Self.F := Self.Header as TIdSipFromToHeader;
end;

//* TestTIdSipFromToHeader Protected methods ***********************************

function TestTIdSipFromToHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipFromToHeader;
end;


//* TestTIdSipFromToHeader Published methods ***********************************

procedure TestTIdSipFromToHeader.TestCopyWithoutTag;
var
  Copy: TIdSipFromToHeader;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=7553452';

  Copy := Self.F.CopyWithoutTag;
  try
    Check(not Copy.HasTag, 'Tag copied');
    Copy.Tag := Self.F.Tag;

    CheckEquals(Self.F.AsString, Copy.AsString, 'Copy and original not equal in other respects');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestHasTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  Check(not Self.F.HasTag, 'No tag');

  Self.F.Tag := BranchMagicCookie + 'f00';
  Check(Self.F.HasTag, 'Tag added via Tag property');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  Check(not Self.F.HasTag, 'No tag sanity check');
  Self.F.Value := 'Case <sip:case@fried.neurons.org>;' + TagParam + '=f00';
  Check(Self.F.HasTag, 'Tag added via SetValue');

  Self.F.Tag := '';
  Check(not Self.F.HasTag, 'Tag set to the empty string after being set by SetValue');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  Self.F.Tag := '';
  Check(not Self.F.HasTag, 'Tag set to the empty string after a SetValue with a no-tag URI');
end;

procedure TestTIdSipFromToHeader.TestIsEqualDifferentURI;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'sip:wintermute@tessier-ashpool.co.luna';
    Check(not Self.F.Equals(From), 'different URI');

    From.Value := 'sips:case@fried.neurons.org';
    Check(not Self.F.Equals(From), 'same user but different scheme; hence different URI');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestIsEqualSameURINoParams;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'Case <sip:case@fried.neurons.org>';
    Check(Self.F.Equals(From), 'Identical headers');

    From.Value := '"Caseless Ammo" <sip:case@fried.neurons.org>';
    Check(Self.F.Equals(From), 'Different display names');

    From.Value := 'sip:case@fried.neurons.org';
    Check(Self.F.Equals(From), 'No display name');

    From.Value := 'sip:case@fried.neurons.org;tag=1';
    Check(not Self.F.Equals(From), 'One has a tag, the other not');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestIsEqualSameURIWithParams;
var
  From: TIdSipFromToHeader;
begin
  Self.F.Value := 'sip:case@fried.neurons.org;tag=1234';

  From := TIdSipFromToHeader.Create;
  try
    From.Value := 'sip:case@fried.neurons.org;x-extend=00;tag=1234';
    Check(Self.F.Equals(From), 'No display name, extension param');

    From.Value := 'sip:case@fried.neurons.org;tag=1234;x-extend=00';
    Check(Self.F.Equals(From),
          'No display name, extension param; order is irrelevant');

    From.Value := 'sip:case@fried.neurons.org;tag=1235';
    Check(not Self.F.Equals(From), 'different tags');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipFromToHeader.TestIsMalformedWithBlankTag;
begin
  Self.F.Value := 'sip:foo;tag=';
  Check(Self.F.IsMalformed, 'From/To tags are not allowed to be blank');
end;

procedure TestTIdSipFromToHeader.TestValue;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  CheckEquals('Case',
              Self.F.DisplayName,
              'DisplayName');
  CheckEquals('sip:case@fried.neurons.org',
              Self.F.Address.Uri,
              'Address');
end;

procedure TestTIdSipFromToHeader.TestValueWithTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  CheckEquals('', Self.F.Tag, '''''');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  CheckEquals('1928301774', Self.F.Tag, '1928301774');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=19283@01774';
  Check(Self.F.IsMalformed,
        'Failed to bail out with malformed token');
end;

procedure TestTIdSipFromToHeader.TestValueResettingTag;
begin
  Self.F.Value := 'Case <sip:case@fried.neurons.org>;tag=1928301774';
  CheckEquals('1928301774', Self.F.Tag, '1928301774');

  Self.F.Value := 'Case <sip:case@fried.neurons.org>';
  CheckEquals('', Self.F.Tag, '''''');
end;

procedure TestTIdSipFromToHeader.TestGetSetTag;
begin
  Self.F.Tag := '123';
  CheckEquals('123', Self.F.Tag, '123');

  Self.F.Tag := '123abc';
  CheckEquals('123abc', Self.F.Tag, '123abc');

  Self.F.Tag := '';
  Check(not Self.F.HasTag, 'Tag wasn''t removed');

  try
    Self.F.Tag := '19283@01774';
    Fail('Failed to bail out with malformed token');
  except
    on EBadHeader do;
  end;
end;

//******************************************************************************
//* TestTIdSipMaxForwardsHeader                                                *
//******************************************************************************
//* TestTIdSipMaxForwardsHeader Public methods *********************************

procedure TestTIdSipMaxForwardsHeader.SetUp;
begin
  inherited SetUp;

  Self.M := Self.Header as TIdSipMaxForwardsHeader;
end;

//* TestTIdSipMaxForwardsHeader Protected methods ******************************

function TestTIdSipMaxForwardsHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipMaxForwardsHeader;
end;

//* TestTIdSipMaxForwardsHeader Published methods ******************************

procedure TestTIdSipMaxForwardsHeader.TestName;
begin
  CheckEquals(MaxForwardsHeader, Self.M.Name, 'Name');

  Self.M.Name := 'foo';
  CheckEquals(MaxForwardsHeader, Self.M.Name, 'Name after set');
end;

procedure TestTIdSipMaxForwardsHeader.TestValue;
begin
  Self.M.Value := '42';
  CheckEquals(42, Self.M.NumericValue, 'NumericValue, 42');

  Self.M.Value := '0';
  CheckEquals(0, Self.M.NumericValue, 'NumericValue, 0');

  Self.M.Value := '255';
  CheckEquals(255, Self.M.NumericValue, 'NumericValue, 255');
end;

procedure TestTIdSipMaxForwardsHeader.TestValueNonNumber;
begin
  Self.M.Value := 'alpha';
  Check(Self.M.IsMalformed,
        'Failed to bail out on non-numeric value for Max-Forwards');
end;

procedure TestTIdSipMaxForwardsHeader.TestValueTooBig;
begin
  Self.M.Value := '256';
  Check(Self.M.IsMalformed,
        'Failed to bail out on numeric value > 255 for Max-Forwards');
end;

procedure TestTIdSipMaxForwardsHeader.TestValueWithParam;
begin
  Self.M.Value := '13;tag=f00';
  Check(Self.M.IsMalformed,
        'Failed to bail out on non-numeric value for Max-Forwards (no params allowed)');
end;

//******************************************************************************
//* TestTIdSipNumericHeader                                                    *
//******************************************************************************
//* TestTIdSipNumericHeader Public methods *************************************

procedure TestTIdSipNumericHeader.SetUp;
begin
  inherited SetUp;

  Self.N := Self.Header as TIdSipNumericHeader;
end;

//* TestTIdSipNumericHeader Protected methods **********************************

function TestTIdSipNumericHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipNumericHeader;
end;

//* TestTIdSipNumericHeader Published methods **********************************

procedure TestTIdSipNumericHeader.TestValue;
begin
  Self.N.Value := '0';
  CheckEquals(0,   Self.N.NumericValue, 'NumericValue (0)');

  Self.N.Value := '666';
  CheckEquals(666, Self.N.NumericValue, 'NumericValue (666)');
end;


procedure TestTIdSipNumericHeader.TestValueWithMultipleTokens;
begin
  Self.N.Value := '1 1';
  Check(Self.N.IsMalformed,
        'Failed to bail out with multiple tokens');
end;

procedure TestTIdSipNumericHeader.TestValueWithNegativeNumber;
begin
  Self.N.Value := '-1';
  Check(Self.N.IsMalformed,
        'Failed to bail out with negative integer');
end;

procedure TestTIdSipNumericHeader.TestValueWithString;
begin
  Self.N.Value := 'one';
  Check(Self.N.IsMalformed,
        'Failed to bail out with string value');
end;

procedure TestTIdSipNumericHeader.TestVeryLargeLegalValue;
begin
  Self.N.Value := '3735928559'; // $deadbeef in base-10
  Check(not Self.N.IsMalformed,
        'Numeric headers must support a sequence number N > (2^32)-1');
end;

procedure TestTIdSipNumericHeader.TestVeryLargeValue;
begin
  Self.N.Value := '9999999999';
  Check(Self.N.IsMalformed,
        'Failed to bail out with a ridiculously large number');
end;

//******************************************************************************
//* TestTIdSipAuthenticateHeader                                               *
//******************************************************************************
//* TestTIdSipAuthenticateHeader Public methods ********************************

procedure TestTIdSipAuthenticateHeader.SetUp;
begin
  inherited SetUp;

  Self.A := Self.Header as TIdSipAuthenticateHeader;
end;

procedure TestTIdSipAuthenticateHeader.TestDomain;
var
  Value: String;
begin
  Value := 'enki.org';
  Self.A.Domain := Value;
  CheckEquals(Value,
              Self.A.Domain,
              Self.ClassName + ' Domain');

  Value := 'tessier-ashpool.co.luna';
  Self.A.Domain := Value;
  CheckEquals(Value,
              Self.A.Domain,
              Self.ClassName + ' Domain');
end;

procedure TestTIdSipAuthenticateHeader.TestName;
begin
  Fail(Self.ClassName + ' must override TestName');
end;

procedure TestTIdSipAuthenticateHeader.TestRemoveStaleResponse;
begin
  Self.A.Stale := true;
  Self.A.RemoveStaleResponse;
  CheckEquals(0,
              Pos('true', Self.A.AsString),
              Self.ClassName + ' Stale response not removed');
end;

procedure TestTIdSipAuthenticateHeader.TestStale;
begin
  Check(not Self.A.Stale,
        Self.ClassName + ' Default value');
  Self.A.Stale := true;
  Check(Self.A.Stale,
        Self.ClassName + ' Should be true');
  Self.A.Stale := false;
  Check(not Self.A.Stale,
        Self.ClassName + ' Should be false');
end;

procedure TestTIdSipAuthenticateHeader.TestValue;
begin
  Self.A.Value := 'Digest realm="enki.org",domain="sip:hiro@enki.org",'
                + 'nonce="123456",opaque="ich bin''s",stale="true",'
                + 'algorithm="SHA1-1024",qop="auth",other-param="foo"';

  CheckEquals('SHA1-1024',
              Self.A.Algorithm,
              Self.ClassName + ' Algorithm');
  CheckEquals('Digest',
              Self.A.AuthorizationScheme,
              Self.ClassName + ' AuthorizationScheme');
  CheckEquals('sip:hiro@enki.org',
              Self.A.Domain,
              Self.ClassName + ' Domain');
  CheckEquals('123456',
              Self.A.Nonce,
              Self.ClassName + ' Nonce');
  CheckEquals('ich bin''s',
              Self.A.Opaque,
              Self.ClassName + ' Opaque');
  CheckEquals('auth',
              Self.A.Qop,
              Self.ClassName + ' Qop');
  CheckEquals('enki.org',
              Self.A.Realm,
              Self.ClassName + ' Realm');
  Check      (Self.A.Stale,
              Self.ClassName + ' Stale');

  Self.A.Value := 'Digest realm="quasinormal.paranoia"';
  CheckEquals('quasinormal.paranoia',
              Self.A.Realm,
              Self.ClassName + ' Realm (only)');
end;

//******************************************************************************
//* TestTIdSipProxyAuthenticateHeader                                          *
//******************************************************************************
//* TestTIdSipProxyAuthenticateHeader Public methods ***************************

procedure TestTIdSipProxyAuthenticateHeader.SetUp;
begin
  inherited SetUp;

  Self.P := Self.Header as TIdSipProxyAuthenticateHeader;
end;

//* TestTIdSipProxyAuthenticateHeader Protected methods ************************

function TestTIdSipProxyAuthenticateHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipProxyAuthenticateHeader;
end;

//* TestTIdSipProxyAuthenticateHeader Published methods ************************

procedure TestTIdSipProxyAuthenticateHeader.TestCredentialHeaderType;
var
  Credential: TIdSipAuthorizationHeader;
begin
  Credential := Self.A.CredentialHeaderType.Create;
  try
    CheckEquals(ProxyAuthorizationHeader,
                Credential.Name,
                Self.A.Name + ' credentials header');
  finally
    Credential.Free;
  end;
end;

procedure TestTIdSipProxyAuthenticateHeader.TestName;
begin
  CheckEquals(ProxyAuthenticateHeader,
              Self.A.Name,
              Self.ClassName + ' Name');
end;

//******************************************************************************
//* TestTIdSipAuthenticationInfoHeader                                         *
//******************************************************************************
//* TestTIdSipAuthenticationInfoHeader Public methods **************************

procedure TestTIdSipAuthenticationInfoHeader.SetUp;
begin
  inherited SetUp;

  Self.A := Self.Header as TIdSipAuthenticationInfoHeader;
end;

//* TestTIdSipAuthenticationInfoHeader Protected methods ***********************

function TestTIdSipAuthenticationInfoHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipAuthenticationInfoHeader;
end;

//* TestTIdSipAuthenticationInfoHeader Published methods ***********************

procedure TestTIdSipAuthenticationInfoHeader.TestCNonce;
var
  Value: String;
begin
  Value := 'f00f00';
  Self.A.CNonce := Value;
  CheckEquals(Value,
              Self.A.CNonce,
              Self.ClassName + ' CNonce');

  Value := 'f00df00d';
  Self.A.CNonce := Value;
  CheckEquals(Value,
              Self.A.CNonce,
              Self.ClassName + ' CNonce');
end;

procedure TestTIdSipAuthenticationInfoHeader.TestHasNextNonce;
begin
  Check(not Self.A.HasNextNonce,
        'NextNonce present in empty header');

  Self.A.NextNonce := 'f00f00';
  Check(Self.A.HasNextNonce,
        'No NextNonce when there should be');

  Self.A.NextNonce := '';
  Check(not Self.A.HasNextNonce,
        'NextNonce when it was removed');
end;

procedure TestTIdSipAuthenticationInfoHeader.TestName;
begin
  Self.A.Name := AuthenticationInfoHeader;

  CheckEquals(AuthenticationInfoHeader,
              Self.A.Name,
              'Name after set');
end;

procedure TestTIdSipAuthenticationInfoHeader.TestNextNonce;
var
  Value: String;
begin
  Value := 'f00f00';
  Self.A.NextNonce := Value;
  CheckEquals(Value,
              Self.A.NextNonce,
              Self.ClassName + ' NextNonce');

  Value := 'f00df00d';
  Self.A.NextNonce := Value;
  CheckEquals(Value,
              Self.A.NextNonce,
              Self.ClassName + ' NextNonce');
end;

procedure TestTIdSipAuthenticationInfoHeader.TestResponseDigest;
var
  Value: String;
begin
  Value := 'f00f00';
  Self.A.ResponseDigest := Value;
  CheckEquals(Value,
              Self.A.ResponseDigest,
              Self.ClassName + ' ResponseDigest');

  Value := 'f00df00d';
  Self.A.ResponseDigest := Value;
  CheckEquals(Value,
              Self.A.ResponseDigest,
              Self.ClassName + ' ResponseDigest');
end;

procedure TestTIdSipAuthenticationInfoHeader.TestValue;
const
  CNonce         = 'deadbeef';
  NextNonce      = 'f00f00';
  NonceCount     = '0000000d';
  Qop            = QopAuthInt;
  ResponseDigest = 'decafbad';
begin
  Self.A.Value := NextNonceParam + '=' + NextNonce + ','
                + QopParam + '=' + Qop + ','
                + ResponseDigestParam + '=' + ResponseDigest + ','
                + CNonceParam + '=' + CNonce + ','
                + NonceCountParam + '=' + NonceCount;
  CheckEquals(NextNonce,
              Self.A.NextNonce,
              'NextNonce');
  CheckEquals(Qop,
              Self.A.Qop,
              'Qop');
  CheckEquals(ResponseDigest,
              Self.A.ResponseDigest,
              'ResponseDigest');
  CheckEquals(CNonce,
              Self.A.CNonce,
              'CNonce');
  CheckEquals(NonceCount,
              Self.A.NC,
              'NonceCount');
end;

//******************************************************************************
//* TestTIdSipProxyAuthorizationHeader                                         *
//******************************************************************************
//* TestTIdSipProxyAuthorizationHeader Public methods **************************

procedure TestTIdSipProxyAuthorizationHeader.SetUp;
begin
  inherited SetUp;

  Self.P := Self.Header as  TIdSipProxyAuthorizationHeader;
end;

//* TestTIdSipProxyAuthorizationHeader Protected methods ***********************

function TestTIdSipProxyAuthorizationHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipProxyAuthorizationHeader;
end;

//* TestTIdSipProxyAuthorizationHeader Published methods ***********************

procedure TestTIdSipProxyAuthorizationHeader.TestName;
begin
  CheckEquals(ProxyAuthorizationHeader,
              Self.P.Name,
              'Name');

  Self.P.Name := 'foo';

  CheckEquals(ProxyAuthorizationHeader,
              Self.P.Name,
              'Name after set');
end;

//******************************************************************************
//* TestTIdSipRetryAfterHeader                                                 *
//******************************************************************************
//* TestTIdSipRetryAfterHeader Public methods **********************************

procedure TestTIdSipRetryAfterHeader.SetUp;
begin
  inherited SetUp;

  Self.R := Self.Header as TIdSipRetryAfterHeader;
end;

//* TestTIdSipRetryAfterHeader Protected methods *******************************

function TestTIdSipRetryAfterHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipRetryAfterHeader;
end;

//* TestTIdSipRetryAfterHeader Published methods *******************************

procedure TestTIdSipRetryAfterHeader.TestGetValue;
begin
  Self.R.NumericValue := 0;
  CheckEquals('0', Self.R.Value, 'Zero Retry-After');

  Self.R.NumericValue := 42;
  CheckEquals('42', Self.R.Value, 'Non-zero numeric value');

  Self.R.Comment := 'No comment';
  CheckEquals('42 (No comment)', Self.R.Value, 'Simple comment');

  Self.R.Comment := '(Nested comment) "And quoted string" ';
  Self.R.Comment := Self.R.Comment + ZeroWidthNonBreakingSpaceChar;
  CheckEquals('42 ((Nested comment) "And quoted string" '
            + Utf8ZeroWidthNonBreakingSpace + ')',
              Self.R.Value,
              'Weird comment');

  Self.R.Comment := 'No comment';
  Self.R.Duration := 22;
  CheckEquals('42 (No comment);duration=22', Self.R.Value, 'Comment + Duration');

  Self.R.Comment := '';
  CheckEquals('42;duration=22', Self.R.Value, 'Duration');

  Self.R.Duration := 0;
  CheckEquals('42', Self.R.Value, 'Duration set to zero');

  Self.R.Params['foo'] := 'bar';
  CheckEquals('42;foo=bar', Self.R.Value, 'General params');
end;

procedure TestTIdSipRetryAfterHeader.TestHasDuration;
begin
  Check(not Self.R.HasDuration,
        'New header');

  Self.R.Value := '1';
  Check(not Self.R.HasDuration,
        'Value = ''1''');

  Self.R.Value := '1 (with comment)';
  Check(not Self.R.HasDuration,
        'Value = ''1 (with comment)''');

  Self.R.Value := '1;duration=0';
  Check(Self.R.HasDuration,
        'Value = ''1;duration=0''');

  Self.R.Value := '1;duration=10';
  Check(Self.R.HasDuration,
        'Value = ''1;duration=10''');
end;

procedure TestTIdSipRetryAfterHeader.TestMalformedValue;
begin
  Self.R.Value := 'a';
  Check(Self.R.IsMalformed,
        'Header not marked as malformed (''a'')');

  Self.R.Value := '1a';
  Check(Self.R.IsMalformed,
        'Header not marked as malformed (''1a'')');
end;

procedure TestTIdSipRetryAfterHeader.TestName;
begin
  CheckEquals(RetryAfterHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(RetryAfterHeader, Self.R.Name, 'Name after set');
end;

procedure TestTIdSipRetryAfterHeader.TestValue;
begin
  Self.R.Value := '1';
  CheckEquals(1,
              Self.R.NumericValue,
              'Value ''1''');

  Self.R.Value := '2';
  CheckEquals(2,
              Self.R.NumericValue,
              'Value ''2''');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithComment;
begin
  Self.R.Value := '1 (Comment Here)';
  CheckEquals(1,
              Self.R.NumericValue,
              'Numeric value');
  CheckEquals('Comment Here',
              Self.R.Comment,
              'Comment');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithCommentAndDuration;
begin
  Self.R.Value := '300 (In a meeting);duration=600';

  Check(not Self.R.IsMalformed,
        'Header marked as invalid: ' + Self.R.ParseFailReason);
  CheckEquals(300,
              Self.R.NumericValue,
              'NumericValue');
  CheckEquals('In a meeting',
              Self.R.Comment,
              'Comment');
  CheckEquals(600,
              Self.R.Duration,
              'Duration');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithDuration;
begin
  Self.R.Value := '1;duration=1000';
  CheckEquals(1000, Self.R.Duration, '1000');

  Self.R.Value := '1;duration=666';
  CheckEquals(666, Self.R.Duration, '666');

  Self.R.Value := '1;duration=0';
  CheckEquals(0, Self.R.Duration, '0');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithOddComments;
begin
  Self.R.Value := '1 (())';
  CheckEquals('()',
              Self.R.Comment,
              '(): ' + Self.R.ParseFailReason);

  Self.R.Value := '1 ("q\\u\(otes\"")';
  CheckEquals('"q\u(otes""',
              Self.R.Comment,
              '"q\\u\(otes\""');
  Check(not Self.R.IsMalformed,
        'Header marked as malformed (''"q\\u\(otes\""''): ' + Self.R.ParseFailReason);

  Self.R.Value := '1 (%30%31%32%33)';
  Check(not Self.R.IsMalformed,
        'Header marked as malformed (''%30%31%32%33''): ' + Self.R.ParseFailReason);
  CheckEquals('%30%31%32%33',
              Self.R.Comment,
              '%30%31%32%33');

  Self.R.Value := '1(a)';
  Check(not Self.R.IsMalformed,
        'Header marked as malformed (''1(a)''): ' + Self.R.ParseFailReason);
  CheckEquals('a',
              Self.R.Comment,
              'a');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithOddWhitespace;
begin
  Self.R.Value := '1'#9'   (Hello)';

  Check(not Self.R.IsMalformed,
        'Retry-After contains a valid string');
  CheckEquals(1,       Self.R.NumericValue, 'NumericValue');
  CheckEquals('Hello', Self.R.Comment,      'Comment');
end;

procedure TestTIdSipRetryAfterHeader.TestValueWithUtf8Comment;
var
  ZeroWidthNonBreakingSpace: WideString;
begin
  // FEFF = zero-width, non-breaking space; in UTF-8 we encode this as
  // EF BB BF
  ZeroWidthNonBreakingSpace := ZeroWidthNonBreakingSpaceChar;

  Self.R.Value := '1 (' + Utf8ZeroWidthNonBreakingSpace + ')';

  CheckEquals(1,
              Self.R.NumericValue,
              'NumericValue');
  CheckEqualsW(ZeroWidthNonBreakingSpace,
               Self.R.Comment,
               'Comment');
end;

//******************************************************************************
//* TRouteHeaderTestCase                                                       *
//******************************************************************************
//* TRouteHeaderTestCase Public methods ****************************************

procedure TRouteHeaderTestCase.SetUp;
begin
  inherited SetUp;

  Self.R := Self.Header as TIdSipRouteHeader;
end;

//* TRouteHeaderTestCase Public methods ****************************************

procedure TRouteHeaderTestCase.TestIsLooseRoutable;
const
  LrOff = '<sip:127.0.0.1>';
  LrOn  = '<sip:127.0.0.1;lr>';
begin
  Self.R.Value := LrOff;
  Check(not Self.R.IsLooseRoutable, 'No lr param');
  Self.R.Value := LrOn;
  Check(Self.R.IsLooseRoutable, 'lr param present');

  Self.R.IsLooseRoutable := false;
  CheckEquals(LrOff, Self.R.Value, 'Set via property; no lr param');

  Self.R.IsLooseRoutable := true;
  CheckEquals(LrOn, Self.R.Value, 'Set via property; lr param present');
end;

procedure TRouteHeaderTestCase.TestValue;
begin
  Self.R.Value := '<sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'URI only: Address');
  CheckEquals('',              Self.R.DisplayName, 'URI only: DisplayName');

  Self.R.Value := 'localhost <sip:127.0.0.1>';
  CheckEquals('sip:127.0.0.1', Self.R.Address.URI, 'display-name: Address');
  CheckEquals('localhost',     Self.R.DisplayName, 'display-name: DisplayName');

  Self.R.Value := '';
  Check(Self.R.IsMalformed,
        'Failed to bail on empty string');

  Self.R.Value := 'sip:127.0.0.1';
  Check(Self.R.IsMalformed,
        'Failed to bail on lack of angle brackets');

  Self.R.Value := '<127.0.0.1>';
  Check(Self.R.IsMalformed,
        'Failed to bail on no scheme');
end;

procedure TRouteHeaderTestCase.TestValueWithLeadingSpaceParam;
begin
  Self.R.Value := '<sip:127.0.0.1>;   unknownParameterWithLeadingLWS=   unknownValueWithLeadingLWS';
  CheckEquals('sip:127.0.0.1',
              Self.R.Address.URI,
              'Address');
  Check(Self.R.HasParameter('unknownParameterWithLeadingLWS'),
        'Parameter not added');
  CheckEquals('unknownValueWithLeadingLWS',
              Self.R.Params['unknownParameterWithLeadingLWS'],
              'Parameter value incorrect');
  CheckEquals('<sip:127.0.0.1>;unknownParameterWithLeadingLWS=unknownValueWithLeadingLWS',
              Self.R.FullValue,
              'FullValue');
end;

procedure TRouteHeaderTestCase.TestValueWithParamsAndHeaderParams;
begin
  Self.R.Value := 'Count Zero <sip:countzero@jacks-bar.com;paranoid>;very';

  CheckEquals('Count Zero', Self.R.DisplayName, 'DisplayName');
  CheckEquals('sip:countzero@jacks-bar.com;paranoid',
              Self.R.Address.URI,
              'Address');
  CheckEquals(';very', Self.R.ParamsAsString, 'Header parameters');
end;

//******************************************************************************
//* TestTIdSipRouteHeader                                                      *
//******************************************************************************
//* TestTIdSipRouteHeader Protected methods ************************************

function TestTIdSipRouteHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipRouteHeader;
end;

//* TestTIdSipRouteHeader Published methods ************************************

procedure TestTIdSipRouteHeader.TestIsLooseRoutable;
begin
  Self.R.Value := '<sip:127.0.0.1>';
  Check(not Self.R.IsLooseRoutable, 'With no lr param');

  Self.R.Value := '<sip:127.0.0.1;lr>';
  Check(Self.R.IsLooseRoutable, 'With lr param');

  Self.R.Value := '<sip:127.0.0.1>;lr';
  Check(not Self.R.IsLooseRoutable, 'With no lr param for header, not URI');
end;

procedure TestTIdSipRouteHeader.TestName;
begin
  CheckEquals(RouteHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(RouteHeader, Self.R.Name, 'Name after set');
end;

//******************************************************************************
//* TestTIdSipRecordRouteHeader                                                *
//******************************************************************************
//* TestTIdSipRecordRouteHeader Protected methods ******************************

function TestTIdSipRecordRouteHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipRecordRouteHeader;
end;

//* TestTIdSipRecordRouteHeader Published methods ******************************

procedure TestTIdSipRecordRouteHeader.TestName;
begin
  CheckEquals(RecordRouteHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(RecordRouteHeader, Self.R.Name, 'Name after set');
end;

//******************************************************************************
//* TestTIdSipReferToHeader                                                    *
//******************************************************************************
//* TestTIdSipReferToHeader Public methods *************************************

procedure TestTIdSipReferToHeader.SetUp;
begin
  inherited SetUp;

  Self.R := Self.Header as TIdSipReferToHeader;
end;

//* TestTIdSipReferToHeader Protected methods **********************************

function TestTIdSipReferToHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipReferToHeader;
end;

//******************************************************************************
//* TestTIdSipParameteredCallIDHeader                                          *
//******************************************************************************
//* TestTIdSipParameteredCallIDHeader Public methods ***************************

procedure TestTIdSipParameteredCallIDHeader.SetUp;
begin
  inherited SetUp;

  Self.C := Self.Header as TIdSipParameteredCallIDHeader;
end;

//* TestTIdSipParameteredCallIDHeader Published methods ************************

procedure TestTIdSipParameteredCallIDHeader.TestCallID;
const
  CallID    = 'foo';
  NewCallID = 'bar';
begin
  Self.C.CallID := CallID;
  CheckEquals(CallID, Self.C.CallID, 'Call-ID not set');

  Self.C.CallID := NewCallID;
  CheckEquals(NewCallID, Self.C.CallID, 'Call-ID not re-set');
end;

procedure TestTIdSipParameteredCallIDHeader.TestSetCallID;
begin
  Self.C.Value := 'foo;from-tag=bar;to-tag=baz';
  Self.C.CallID := 'quaax';

  Check(Self.C.HasParameter(FromTagParam), FromTagParam + ' removed');
  Check(Self.C.HasParameter(ToTagParam),   ToTagParam + ' removed');
end;

procedure TestTIdSipParameteredCallIDHeader.TestValue;
begin
  Self.C.Value := 'foo;from-tag=bar;to-tag=baz';

  CheckEquals('foo', Self.C.CallID, 'Call-ID');

  Check(Self.C.HasParameter(FromTagParam), 'No "' + FromTagParam + '"');
  CheckEquals('bar',
              Self.C.Params[FromTagParam],
              '"' + FromTagParam + '" value');

  Check(Self.C.HasParameter(ToTagParam), 'No "' + ToTagParam + '"');
  CheckEquals('bar',
              Self.C.Params[ToTagParam],
              '"' + ToTagParam + '" value');
  CheckEquals(2, Self.C.ParamCount, 'Parameter count (' + Self.C.FullValue + ')');
end;

//******************************************************************************
//* TestTIdSipReplacesHeader                                                   *
//******************************************************************************
//* TestTIdSipReplacesHeader Public methods ************************************

procedure TestTIdSipReplacesHeader.SetUp;
begin
  inherited SetUp;

  Self.R := Self.Header as TIdSipReplacesHeader;
end;

//* TestTIdSipReplacesHeader Protected methods *********************************

function TestTIdSipReplacesHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipReplacesHeader;
end;

//* TestTIdSipReplacesHeader Published methods *********************************

procedure TestTIdSipReplacesHeader.TestFromTag;
const
  NewTag = 'bar';
  Tag    = 'foo';
begin
  Self.R.FromTag := Tag;
  CheckEquals(Tag, Self.R.FromTag, 'From tag not set');

  Self.R.FromTag := NewTag;
  CheckEquals(NewTag, Self.R.FromTag, 'From tag not re-set');

  Self.R.Value := 'arbcallid;from-tag=1';
  Self.R.FromTag := NewTag;
  CheckEquals(NewTag, Self.R.FromTag, 'From tag not re-set after SetValue');
end;

procedure TestTIdSipReplacesHeader.TestIsEarly;
begin
  Check(not Self.R.IsEarly, 'Initial value of IsEarly');

  Self.R.Value := 'arbcallid;' + EarlyOnlyParam;
  Check(Self.R.IsEarly,
        EarlyOnlyParam + ' present but IsEarly returns wrong value');
end;

procedure TestTIdSipReplacesHeader.TestName;
begin
  CheckEquals(ReplacesHeader, Self.R.Name, 'Name');

  Self.R.Name := 'foo';
  CheckEquals(ReplacesHeader, Self.R.Name, 'Name after set');
end;

procedure TestTIdSipReplacesHeader.TestToTag;
const
  NewTag = 'bar';
  Tag    = 'foo';
begin
  Self.R.ToTag := Tag;
  CheckEquals(Tag, Self.R.ToTag, 'To tag not set');

  Self.R.ToTag := NewTag;
  CheckEquals(NewTag, Self.R.ToTag, 'To tag not re-set');
end;

procedure TestTIdSipReplacesHeader.TestValue;
const
  CallID  = 'foo';
  FromTag = 'bar';
  ToTag   = 'baz';
begin
  Self.R.Value := Format('%s;from-tag=%s;to-tag=%s', [CallID, FromTag, ToTag]);
  CheckEquals(CallID,  Self.R.CallID,  CallIDHeaderFull);
  CheckEquals(FromTag, Self.R.FromTag, FromTagParam);
  CheckEquals(ToTag,   Self.R.ToTag,   ToTagParam);
  CheckEquals(Self.R.CallID,
              Self.R.Value,
              '"Value" should equal the Call-ID to avoid confusion in, say, FullValue');

  Check(not Self.R.IsMalformed,
        'Syntactically correct header claims to be malformed');
end;

procedure TestTIdSipReplacesHeader.TestValueMissingFromTag;
begin
  Self.R.Value := 'arbcallid;to-tag=1';

  Check(Self.R.IsMalformed,
        'Header with missing from-tag not marked as malformed');
  Check(Pos(FromTagParam, Self.R.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.R.ParseFailReason);
end;

procedure TestTIdSipReplacesHeader.TestValueMissingToTag;
begin
  Self.R.Value := 'arbcallid;from-tag=1';

  Check(Self.R.IsMalformed,
        'Header with missing to-tag not marked as malformed');
  Check(Pos(ToTagParam, Self.R.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.R.ParseFailReason);
end;

procedure TestTIdSipReplacesHeader.TestValueMultipleFromTags;
begin
  Self.R.Value := 'arbcallid;from-tag=1;from-tag=2;to-tag=3';
  Check(Self.R.IsMalformed,
        'Header with two from-tags not marked as malformed');
  Check(Pos(FromTagParam, Self.R.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.R.ParseFailReason);
end;

procedure TestTIdSipReplacesHeader.TestValueMultipleToTags;
begin
  Self.R.Value := 'arbcallid;to-tag=1;from-tag=2;to-tag=3';
  Check(Self.R.IsMalformed,
        'Header with two to-tags not marked as malformed');
  Check(Pos(ToTagParam, Self.R.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.R.ParseFailReason);
end;

//******************************************************************************
//* TestTIdSipSubscriptionStateHeader                                          *
//******************************************************************************
//* TestTIdSipSubscriptionStateHeader Public methods ***************************

procedure TestTIdSipSubscriptionStateHeader.SetUp;
begin
  inherited SetUp;

  Self.SS := Self.Header as TIdSipSubscriptionStateHeader;
end;

//* TestTIdSipSubscriptionStateHeader Protected methods ************************

function TestTIdSipSubscriptionStateHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipSubscriptionStateHeader;
end;

//* TestTIdSipSubscriptionStateHeader Published methods ************************

procedure TestTIdSipSubscriptionStateHeader.TestAssign;
var
  Other: TIdSipSubscriptionStateHeader;
begin
  Self.SS.Value := 'terminated;reason=noresource';

  Other := TIdSipSubscriptionStateHeader.Create;
  try
    Other.Assign(Self.SS);
    
    CheckEquals(Self.SS.FullValue,
                Other.FullValue,
                'FullValue');
    CheckEquals(Self.SS.Reason,
                Other.Reason,
                'Reason');
    CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(Self.SS.ReasonType),
                TIdSipSubscriptionStateHeader.ReasonTypeToStr(Other.ReasonType),
                'ReasonType');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipSubscriptionStateHeader.TestGetReasonType;
begin
  CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrDeactivated),
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(Self.SS.ReasonType),
              'Initial value');

  Self.SS.Reason := 'unknown reason';

  CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrUnknownReason),
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(Self.SS.ReasonType),
             'ReasonType not set after SetReason');

  Self.SS.Reason := '';

  CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrNoReason),
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(Self.SS.ReasonType),
             'ReasonType not set after 2nd SetReason');
end;

procedure TestTIdSipSubscriptionStateHeader.TestHasGivenUp;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.HasGivenUp, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.HasGivenUp, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.HasGivenUp, 'Terminated subscription');

  Self.SS.Reason := EventReasonRejected;
  Check(not Self.SS.HasGivenUp, 'Terminated subscription; reason: rejected');

  Self.SS.Reason := EventReasonGiveUp;
  Check(Self.SS.HasGivenUp, 'Terminated subscription; reason: giveup');
end;

procedure TestTIdSipSubscriptionStateHeader.TestHasRetryAfter;
begin
  Self.SS.Value := SubscriptionSubstateTerminated;
  Check(not Self.SS.HasRetryAfter,
        'Has no Retry-After: ''' + Self.SS.Value + '''');

  Self.SS.Value := SubscriptionSubstateTerminated + ';' + RetryAfterParam + '=100';
  Check(Self.SS.HasRetryAfter,
        'Has no Retry-After: ''' + Self.SS.Value + '''');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsActive;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsActive, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(Self.SS.IsActive, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsActive, 'Terminated subscription');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsDeactivated;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsDeactivated, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsDeactivated, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsDeactivated, 'Terminated subscription');

  Self.SS.Reason := EventReasonRejected;
  Check(not Self.SS.IsDeactivated, 'Terminated subscription; reason: rejected');

  Self.SS.Reason := EventReasonDeactivated;
  Check(Self.SS.IsDeactivated, 'Terminated subscription; reason: deactivated');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsInProbation;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsInProbation, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsInProbation, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsInProbation, 'Terminated subscription');

  Self.SS.Reason := EventReasonRejected;
  Check(not Self.SS.IsInProbation, 'Terminated subscription; reason: rejected');

  Self.SS.Reason := EventReasonProbation;
  Check(Self.SS.IsInProbation, 'Terminated subscription; reason: probation');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsNoResource;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsNoResource, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsNoResource, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsNoResource, 'Terminated subscription');

  Self.SS.Reason := EventReasonTimeout;
  Check(not Self.SS.IsNoResource, 'Terminated subscription; reason: timeout');

  Self.SS.Reason := EventReasonNoResource;
  Check(Self.SS.IsNoResource, 'Terminated subscription; reason: noresource');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsPending;
begin
  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsPending, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstatePending;
  Check(Self.SS.IsPending, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsPending, 'Terminated subscription');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsRejected;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsRejected, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsRejected, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsRejected, 'Terminated subscription');

  Self.SS.Reason := EventReasonTimeout;
  Check(not Self.SS.IsRejected, 'Terminated subscription; reason: timeout');

  Self.SS.Reason := EventReasonRejected;
  Check(Self.SS.IsRejected, 'Terminated subscription; reason: rejected');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsTerminated;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsTerminated, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsTerminated, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(Self.SS.IsTerminated, 'Terminated subscription');
end;

procedure TestTIdSipSubscriptionStateHeader.TestIsTimedOut;
begin
  Self.SS.SubState := SubscriptionSubstatePending;
  Check(not Self.SS.IsTimedOut, 'Pending subscription');

  Self.SS.SubState := SubscriptionSubstateActive;
  Check(not Self.SS.IsTimedOut, 'Active subscription');

  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(not Self.SS.IsTimedOut, 'Terminated subscription');

  Self.SS.Reason := EventReasonRejected;
  Check(not Self.SS.IsTimedOut, 'Terminated subscription; reason: rejected');

  Self.SS.Reason := EventReasonTimeout;
  Check(Self.SS.IsTimedOut, 'Terminated subscription; reason: timeout');
end;

procedure TestTIdSipSubscriptionStateHeader.TestReasonTypeToStr;
begin
  CheckEquals(EventReasonDeactivated,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrDeactivated),
              EventReasonDeactivated);
  CheckEquals(EventReasonGiveUp,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrGiveUp),
              EventReasonGiveUp);
  CheckEquals('',
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrNoReason),
              'No reason');
  CheckEquals(EventReasonNoResource,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrNoResource),
              EventReasonNoResource);
  CheckEquals(EventReasonProbation,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrProbation),
              EventReasonProbation);
  CheckEquals(EventReasonRejected,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrRejected),
              EventReasonRejected);
  CheckEquals(EventReasonTimeout,
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrTimeout),
              EventReasonTimeout);
  CheckEquals('',
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrUnknownReason),
              'Unknown reason');
end;

procedure TestTIdSipSubscriptionStateHeader.TestRetryAfterHasMeaning;
begin
  Self.SS.SubState := SubscriptionSubstateTerminated;
  Check(Self.SS.RetryAfterHasMeaning, 'No reason');

  Self.SS.Reason := 'unknown-reason';
  Check(Self.SS.RetryAfterHasMeaning, 'Unknown reason');

  Self.SS.Reason := EventReasonDeactivated;
  Check(not Self.SS.RetryAfterHasMeaning, EventReasonDeactivated);

  Self.SS.Reason := EventReasonProbation;
  Check(Self.SS.RetryAfterHasMeaning, EventReasonProbation);

  Self.SS.Reason := EventReasonRejected;
  Check(not Self.SS.RetryAfterHasMeaning, EventReasonRejected);

  Self.SS.Reason := EventReasonTimeout;
  Check(not Self.SS.RetryAfterHasMeaning, EventReasonTimeout);

  Self.SS.Reason := EventReasonGiveUp;
  Check(Self.SS.RetryAfterHasMeaning, EventReasonGiveUp);

  Self.SS.Reason := EventReasonNoResource;
  Check(not Self.SS.RetryAfterHasMeaning, EventReasonNoResource);
end;

procedure TestTIdSipSubscriptionStateHeader.TestSetReasonType;
const
  ArbReason = 'arbitrary_reason';
var
  RT: TIdSipSubscriptionStateReason;
begin
  for RT := Low(TIdSipSubscriptionStateReason) to High(TIdSipSubscriptionStateReason) do begin
    Self.SS.ReasonType := RT;

    Check(Self.SS.ReasonType = RT,
          'ReasonType after setting to ' + TIdSipSubscriptionStateHeader.ReasonTypeToStr(RT));

    if (Self.SS.ReasonType <> ssrUnknownReason) then
      CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(RT),
                  Self.SS.Reason,
                  TIdSipSubscriptionStateHeader.ReasonTypeToStr(RT));
  end;

  Self.SS.Reason := ArbReason;
  Self.SS.ReasonType := ssrUnknownReason;
  CheckEquals(ArbReason,
              Self.SS.Reason,
              'Setting ReasonType to ssrUnknownReason leaves Reason unaltered');
end;

procedure TestTIdSipSubscriptionStateHeader.TestStrToReasonType;
const
  UnknownReason = 'unknown_reason';
begin
  Check(ssrDeactivated = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonDeactivated),
        EventReasonDeactivated);
  Check(ssrGiveUp = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonGiveUp),
        EventReasonGiveUp);
  Check(ssrNoReason = TIdSipSubscriptionStateHeader.StrToReasonType(''),
        'No reason');
  Check(ssrNoResource = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonNoResource),
        EventReasonNoResource);
  Check(ssrProbation = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonProbation),
        EventReasonProbation);
  Check(ssrRejected = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonRejected),
        EventReasonRejected);
  Check(ssrTimeout = TIdSipSubscriptionStateHeader.StrToReasonType(EventReasonTimeout),
        EventReasonTimeout);
  Check(ssrUnknownReason = TIdSipSubscriptionStateHeader.StrToReasonType(UnknownReason),
        UnknownReason);
end;

procedure TestTIdSipSubscriptionStateHeader.TestValue;
begin
  Self.SS.Value := 'pending;expires=1;reason=probation;retry-after=2';

  CheckEquals(SubscriptionSubstatePending, Self.SS.SubState,   'SubState');
  CheckEquals(1,                           Self.SS.Expires,    'Expires');
  CheckEquals(EventReasonProbation,        Self.SS.Reason,     'Reason');
  CheckEquals(TIdSipSubscriptionStateHeader.ReasonTypeToStr(ssrProbation),
              TIdSipSubscriptionStateHeader.ReasonTypeToStr(Self.SS.ReasonType),
              'ReasonType');
  CheckEquals(2,                           Self.SS.RetryAfter, 'RetryAfter');
end;

//******************************************************************************
//* TestTIdSipTargetDialogHeader                                               *
//******************************************************************************
//* TestTIdSipTargetDialogHeader Public methods ********************************

procedure TestTIdSipTargetDialogHeader.SetUp;
begin
  inherited SetUp;

  Self.T := Self.Header as TIdSipTargetDialogHeader;
end;

//* TestTIdSipTargetDialogHeader Protected methods *****************************

function TestTIdSipTargetDialogHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipTargetDialogHeader;
end;

//* TestTIdSipTargetDialogHeader Published methods *****************************

procedure TestTIdSipTargetDialogHeader.TestAssignDoesntNormallyRaiseAnException;
var
  Other: TIdSipTargetDialogHeader;
begin
  Other := TIdSipTargetDialogHeader.Create;
  try
    Other.Value := '1;local-tag=2;remote-tag=3';

    Self.T.Assign(Other);
    Check(not Self.T.IsMalformed,
          'Header marked as malformed; the Assign() didn''t work properly');
    Check(Self.T.Equals(Other), 'T <> Other; Assign() didn''t');
    CheckEquals(Other.AsString,
                Self.T.AsString,
                'Header claims to be well-formed but Assign() didn''t copy '
              + 'everything');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipTargetDialogHeader.TestHasCompleteDialogID;
begin
  Check(not Self.T.HasCompleteDialogID,
        'Blank call-id, no local-tag, no remote-tag');

  Self.T.CallID := 'foo';
  Check(not Self.T.HasCompleteDialogID,
        'No local-tag, no remote-tag');

  Self.T.LocalTag := 'bar';
  Check(not Self.T.HasCompleteDialogID,
        'No remote-tag');

  Self.T.RemoteTag := 'baz';
  Check(Self.T.HasCompleteDialogID,
        'With complete dialog ID');

  Self.T.RemoveParameter(LocalTagParam);
  Check(not Self.T.HasCompleteDialogID,
        'No local-tag');
end;

procedure TestTIdSipTargetDialogHeader.TestLocalTag;
const
  NewTag = 'bar';
  Tag    = 'foo';
begin
  Self.T.LocalTag := Tag;
  CheckEquals(Tag, Self.T.LocalTag, 'Local tag not set');

  Self.T.LocalTag := NewTag;
  CheckEquals(NewTag, Self.T.LocalTag, 'Local tag not re-set');

  Self.T.Value := 'arbcallid;local-tag=1';
  Self.T.LocalTag := NewTag;
  CheckEquals(NewTag, Self.T.LocalTag, 'Local tag not re-set after SetValue');
end;

procedure TestTIdSipTargetDialogHeader.TestName;
begin
  CheckEquals(TargetDialogHeader, Self.T.Name, 'Name');

  Self.T.Name := 'foo';
  CheckEquals(TargetDialogHeader, Self.T.Name, 'Name after set');
end;

procedure TestTIdSipTargetDialogHeader.TestRemoteTag;
const
  NewTag = 'bar';
  Tag    = 'foo';
begin
  Self.T.RemoteTag := Tag;
  CheckEquals(Tag, Self.T.RemoteTag, 'Remote tag not set');

  Self.T.RemoteTag := NewTag;
  CheckEquals(NewTag, Self.T.RemoteTag, 'Remote tag not re-set');

  Self.T.Value := 'arbcallid;Remote-tag=1';
  Self.T.RemoteTag := NewTag;
  CheckEquals(NewTag, Self.T.RemoteTag, 'Remote tag not re-set after SetValue');
end;

procedure TestTIdSipTargetDialogHeader.TestValue;
const
  CallID    = 'foo';
  LocalTag  = 'bar';
  RemoteTag = 'baz';
begin
  Self.T.Value := Format('%s;local-tag=%s;remote-tag=%s', [CallID, LocalTag, RemoteTag]);
  CheckEquals(CallID,    Self.T.CallID,  CallIDHeaderFull);
  CheckEquals(LocalTag,  Self.T.LocalTag, LocalTagParam);
  CheckEquals(RemoteTag, Self.T.RemoteTag,   RemoteTagParam);

  Check(not Self.T.IsMalformed,
        'Syntactically correct header claims to be malformed');
end;

procedure TestTIdSipTargetDialogHeader.TestValueMissingLocalTag;
begin
  Self.T.Value := 'foo;remote-tag=baz';

  Check(Self.T.IsMalformed,
        'Header with missing local-tag not marked as malformed');
  Check(Pos(LocalTagParam, Self.T.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.T.ParseFailReason);
end;

procedure TestTIdSipTargetDialogHeader.TestValueMissingRemoteTag;
begin
  Self.T.Value := 'foo;local-tag=baz';

  Check(Self.T.IsMalformed,
        'Header with missing Remote-tag not marked as malformed');
  Check(Pos(RemoteTagParam, Self.T.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.T.ParseFailReason);
end;

procedure TestTIdSipTargetDialogHeader.TestValueMultipleLocalTags;
begin
  Self.T.Value := 'arbcallid;local-tag=1;local-tag=2;remote-tag=3';

  Check(Self.T.IsMalformed,
        'Header with two local-tags not marked as malformed');
  Check(Pos(LocalTagParam, Self.T.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.T.ParseFailReason);
end;

procedure TestTIdSipTargetDialogHeader.TestValueMultipleRemoteTags;
begin
  Self.T.Value := 'arbcallid;remote-tag=1;remote-tag=2;local-tag=3';

  Check(Self.T.IsMalformed,
        'Header with two remote-tags not marked as malformed');
  Check(Pos(RemoteTagParam, Self.T.ParseFailReason) > 0,
        'Insufficiently informative ParseFailReason: ' + Self.T.ParseFailReason);
end;

//******************************************************************************
//* TestTIdSipTimestampHeader                                                  *
//******************************************************************************
//* TestTIdSipTimestampHeader Public methods ***********************************

procedure TestTIdSipTimestampHeader.SetUp;
begin
  inherited SetUp;

  Self.T := Self.Header as TIdSipTimestampHeader;
end;

//* TestTIdSipTimestampHeader Protected methods ********************************

function TestTIdSipTimestampHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipTimestampHeader;
end;

//* TestTIdSipTimestampHeader Published methods ********************************

procedure TestTIdSipTimestampHeader.TestName;
begin
  CheckEquals(TimestampHeader, Self.T.Name, 'Name');

  Self.T.Name := 'foo';
  CheckEquals(TimestampHeader, Self.T.Name, 'Name after set');
end;

procedure TestTIdSipTimestampHeader.TestNormalizeLWS;
begin
  CheckEquals('',       Self.T.NormalizeLWS(''),             '''''');
  CheckEquals('hello',  Self.T.NormalizeLWS('hello'),        'hello');
  CheckEquals('a b',    Self.T.NormalizeLWS('a b'),          'a b');
  CheckEquals('a b c',  Self.T.NormalizeLWS('a b  c'),       'a b  c');
  CheckEquals('a b',    Self.T.NormalizeLWS('a'#9'b'),       'a TAB b');
  CheckEquals('a b',    Self.T.NormalizeLWS('a'#13#10'  b'), 'a CRLF SP SP b');
end;

procedure TestTIdSipTimestampHeader.TestReadNumber;
var
  Src: String;
begin
  Src := '1';
  CheckEquals(1, Self.T.ReadNumber(Src), '1');
  CheckEquals('', Src, 'Src after ''1''');

  Src := '123 ';
  CheckEquals(123, Self.T.ReadNumber(Src), '123 SP');
  CheckEquals(' ', Src, 'Src after ''123 ''');

  Src := '456.';
  CheckEquals(456, Self.T.ReadNumber(Src), '456.');
  CheckEquals('.', Src, 'Src after ''456.''');

  Src := '';
  try
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on empty string');
  except
    on EBadHeader do;
  end;

  Src := 'a';
  try
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on non-number');
  except
    on EBadHeader do;
  end;

  Src := 'a 1';
  try
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on non-number, SP, number');
  except
    on EBadHeader do;
  end;

  Src := '-66';
  try
    Self.T.ReadNumber(Src);
    Fail('Failed to bail out on negative number (hence non-number)');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipTimestampHeader.TestValue;
begin
  Self.T.Value := '1';
  CheckEquals(1, Self.T.Timestamp.IntegerPart,    '1: Timestamp.IntegerPart');
  CheckEquals(0, Self.T.Timestamp.FractionalPart, '1: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '1: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '1: Delay.FractionalPart');

  Self.T.Value := '99.';
  CheckEquals(99, Self.T.Timestamp.IntegerPart,    '2: Timestamp.IntegerPart');
  CheckEquals(0,  Self.T.Timestamp.FractionalPart, '2: Timestamp.FractionalPart');
  CheckEquals(0,  Self.T.Delay.IntegerPart,        '2: Delay.IntegerPart');
  CheckEquals(0,  Self.T.Delay.FractionalPart,     '2: Delay.FractionalPart');

  Self.T.Value := '2.2';
  CheckEquals(2, Self.T.Timestamp.IntegerPart,    '3: Timestamp.IntegerPart');
  CheckEquals(2, Self.T.Timestamp.FractionalPart, '3: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '3: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '3: Delay.FractionalPart');
end;

procedure TestTIdSipTimestampHeader.TestValueMalformed;
begin
  Self.T.Value := 'a';
  Check(Self.T.IsMalformed,
        'Failed to bail out on non-integer');

  Self.T.Value := '1..1';
  Check(Self.T.IsMalformed,
        'Failed to bail out on too many periods');

  Self.T.Value := '.1';
  Check(Self.T.IsMalformed,
        'Failed to bail out on no digits before period');

  Self.T.Value := '1 a';
  Check(Self.T.IsMalformed,
        'Failed to bail out on malformed delay');

  Self.T.Value := '1 1.1;tag';
  Check(Self.T.IsMalformed,
        'Failed to bail out on params');
end;

procedure TestTIdSipTimestampHeader.TestValueWithDelay;
begin
  Self.T.Value := '5.1 3';
  CheckEquals(5, Self.T.Timestamp.IntegerPart,    '1: Timestamp.IntegerPart');
  CheckEquals(1, Self.T.Timestamp.FractionalPart, '1: Timestamp.FractionalPart');
  CheckEquals(3, Self.T.Delay.IntegerPart,        '1: Delay.IntegerPart');
  CheckEquals(0, Self.T.Delay.FractionalPart,     '1: Delay.FractionalPart');

  Self.T.Value := '1.2 3.4';
  CheckEquals(1, Self.T.Timestamp.IntegerPart,    '2: Timestamp.IntegerPart');
  CheckEquals(2, Self.T.Timestamp.FractionalPart, '2: Timestamp.FractionalPart');
  CheckEquals(3, Self.T.Delay.IntegerPart,        '2: Delay.IntegerPart');
  CheckEquals(4, Self.T.Delay.FractionalPart,     '2: Delay.FractionalPart');

  Self.T.Value := '6.5 .4';
  CheckEquals(6, Self.T.Timestamp.IntegerPart,    '3: Timestamp.IntegerPart');
  CheckEquals(5, Self.T.Timestamp.FractionalPart, '3: Timestamp.FractionalPart');
  CheckEquals(0, Self.T.Delay.IntegerPart,        '3: Delay.IntegerPart');
  CheckEquals(4, Self.T.Delay.FractionalPart,     '3: Delay.FractionalPart');
end;

//******************************************************************************
//* TestTIdSipUriHeader                                                        *
//******************************************************************************
//* TestTIdSipUriHeader Public methods *****************************************

procedure TestTIdSipUriHeader.SetUp;
begin
  inherited SetUp;

  Self.U := Self.Header as TIdSipUriHeader;
  Self.U.Name := FromHeaderFull; // Just a random URI-using header.
end;

//* TestTIdSipUriHeader Protected methods **************************************

function TestTIdSipUriHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipUriHeader;
end;

//* TestTIdSipUriHeader Published methods **************************************

procedure TestTIdSipUriHeader.TestEqualsDifferentHeader;
var
  Other: TIdSipUriHeader;
begin
  Self.U.Value := '<sip:example.com>';

  Other := TIdSipUriHeader.Create;
  try
    Other.Name := 'X-' + Self.U.Name;
    Other.Value := Self.U.Value;

    Check(not Self.U.Equals(Other), 'This = Other');
    Check(not Other.Equals(Self.U), 'Other = This');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipUriHeader.TestEqualsDifferentURI;
var
  Other: TIdSipUriHeader;
begin
  Self.U.Value := '<sip:example.com>';

  Other := TIdSipUriHeader.Create;
  try
    Other.Name  := Self.U.Name;
    Other.Value := Self.U.Value;
    Other.Address.Username := Other.Address.Username + '-1';

    Check(not Self.U.Equals(Other), 'This = Other');
    Check(not Other.Equals(Self.U), 'Other = This');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipUriHeader.TestEqualsSameHeaderValueDifferentClass;
var
  Other: TIdSipHeader;
begin
  Self.U.Value := '<sip:example.com>';

  Other := TIdSipHeader.Create;
  try
    Other.Name  := Self.U.Name;
    Other.Value := Self.U.Value;

    Check(Self.U.Equals(Other), 'This <> Other');
    Check(Other.Equals(Self.U), 'Other <> This');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipUriHeader.TestEqualsSameURINoParams;
var
  Other: TIdSipUriHeader;
begin
  Self.U.Value := '<sip:example.com>';

  Other := TIdSipUriHeader.Create;
  try
    Other.Name  := Self.U.Name;
    Other.Value := Self.U.Value;

    Check(Self.U.Equals(Other), 'This <> Other');
    Check(Other.Equals(Self.U), 'Other <> This');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipUriHeader.TestEqualsSameURIWithParams;
var
  Other: TIdSipUriHeader;
begin
  Self.U.Value := '<sip:example.com>';

  Other := TIdSipUriHeader.Create;
  try
    Other.Name  := Self.U.Name;
    Other.Value := Self.U.Value;

    // Same parameters, but different order.
    Self.U.Params['foo'] := '1';
    Self.U.Params['bar'] := '1';
    Other.Params['bar']  := '1';
    Other.Params['foo']  := '1';

    Check(Self.U.Equals(Other), 'This <> Other');
    Check(Other.Equals(Self.U), 'Other <> This');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSipUriHeader.TestIsGruu;
const
  BaseUri = 'sip:wintermute@tessier-ashpool.co.luna';
begin
  Self.U.Address.Uri := BaseUri;
  Check(not Self.U.IsGruu, 'Looks like a GRUU, but no "gruu" parameter');

  Self.U.IsGruu := true;
  Check(Self.U.IsGruu, 'IsGruu not set');
  CheckEquals(BaseUri + ';' + GruuParam,
              Self.U.Address.Uri,
              '"gruu" parameter doesn''t show up in the URI');

  Self.U.IsGruu := false;
  Check(not Self.U.IsGruu, 'IsGruu not unset');
  CheckEquals(BaseUri,
              Self.U.Address.Uri,
              '"gruu" parameter not removed');

  Self.U.Address.Uri := BaseUri + ';' + GruuParam;
  Check(Self.U.IsGruu,
        'IsGruu not set after setting Uri with a GRUU');
end;

procedure TestTIdSipUriHeader.TestValue;
begin
  Self.U.Value := '<sip:case@jacks-bar.com>';
  CheckEquals('sip:case@jacks-bar.com',
              Self.U.Address.URI,
              'Address.GetFullURI');

  Self.U.Value := '';
  Check(Self.U.IsMalformed,
        'Empty string');

  Self.U.Value := 'sip:case@jacks-bar.com';
  Check(Self.U.IsMalformed,
        'un <>''d URI');

  Self.U.Value := 'Case <sip:case@jacks-bar.com>';
  Check(Self.U.IsMalformed,
        'No display names allowed');
end;

procedure TestTIdSipUriHeader.TestValueWithParams;
begin
  Self.U.Value := '<sip:case@jacks-bar.com>;value=name';
  CheckEquals(1,             Self.U.ParamCount,     'ParamCount');
  CheckEquals(';value=name', Self.U.ParamsAsString, 'ParamsAsString');
end;

procedure TestTIdSipUriHeader.TestValueWithUriParams;
begin
  Self.U.Value := '<sip:case@jacks-bar.com;value=name>';
  CheckEquals(0, Self.U.ParamCount, 'ParamCount');
  CheckEquals('sip:case@jacks-bar.com;value=name',
              Self.U.Address.URI,
              'Address');
end;

//******************************************************************************
//* TestTIdSipViaHeader                                                        *
//******************************************************************************
//* TestTIdSipViaHeader Public methods *****************************************

procedure TestTIdSipViaHeader.SetUp;
begin
  inherited SetUp;

  Self.V := TIdSipViaHeader.Create;

  TIdSipTransportRegistry.RegisterTransportType(TlsOverSctpTransport, TIdSipMockTlsOverSctpTransport);
end;

procedure TestTIdSipViaHeader.TearDown;
begin
  TIdSipTransportRegistry.UnregisterTransportType(TlsOverSctpTransport);

  inherited TearDown;
end;

//* TestTIdSipViaHeader Protected methods **************************************

function TestTIdSipViaHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipViaHeader;
end;

//* TestTIdSipViaHeader Published methods **************************************

procedure TestTIdSipViaHeader.TestAssign;
var
  V2: TIdSipViaHeader;
begin
  V2 := TIdSipViaHeader.Create;
  try
    Self.V.Value := 'SIP/7.0/SCTP localhost;branch=' + BranchMagicCookie + 'f00';
    V2.Assign(Self.V);
    Check(V2.Equals(Self.V), 'V2 not properly assigned to');

    Self.V.IsUnset := true;
    V2.Assign(Self.V);
    Check(V2.IsUnset, 'IsUnset not copied');
  finally
    V2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestAssignWithDefaultPortSpecified;
var
  V2: TIdSipViaHeader;
begin
  V2 := TIdSipViaHeader.Create;
  try
    Self.V.Value := 'SIP/2.0/TCP localhost:5060;branch=' + BranchMagicCookie + 'f00';
    V2.Assign(Self.V);

    CheckEquals(Self.V.Value,
                V2.Value,
                'V2 not properly assigned to');
  finally
    V2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestAsUri;
type
  TestData = record
    Reference: String;
    SentBy:    String;
  end;
const
  SentBys: array[1..3] of TestData =
          ((Reference: 'FQDN';           SentBy: 'gw1.leo-ix.net'),
           (Reference: 'IP';             SentBy: '127.0.0.1'),
           (Reference: 'IPv6 reference'; SentBy: '[::1]'));
var
  I: Integer;
begin
  for I := Low(SentBys) to High(SentBys) do begin
    Self.V.Value := 'SIP/2.0/UDP ' + SentBys[I].SentBy;
    CheckEquals('sip:' + SentBys[I].SentBy,
                Self.V.AsUri,
                SentBys[I].Reference);

    Self.V.Port := 2020;
    CheckEquals('sip:' + SentBys[I].SentBy + ':' + IntToStr(Self.V.Port),
                Self.V.AsUri,
                SentBys[I].Reference + ' + port');
  end;

  for I := Low(SentBys) to High(SentBys) do begin
    Self.V.Value := 'SIP/2.0/TLS ' + SentBys[I].SentBy;
    CheckEquals('sips:' + SentBys[I].SentBy,
                Self.V.AsUri,
                SentBys[I].Reference);

    Self.V.Port := 2020;
    CheckEquals('sips:' + SentBys[I].SentBy + ':' + IntToStr(Self.V.Port),
                Self.V.AsUri,
                SentBys[I].Reference + ' + port');
  end;
end;

procedure TestTIdSipViaHeader.TestBranch;
begin
  Self.V.Branch := BranchMagicCookie;
  CheckEquals(BranchMagicCookie,
              Self.V.Branch,
              BranchMagicCookie);

  Self.V.Branch := BranchMagicCookie + 'abcdef';
  CheckEquals(BranchMagicCookie + 'abcdef',
              Self.V.Branch,
              BranchMagicCookie + 'abcdef');

  try
    Self.V.Branch := '';
    Fail('Failed to bail out on an invalid branch - empty string');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Branch := 'I am not a valid branch';
    Fail('Failed to bail out on an invalid branch - multiple tokens');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestHasBranch;
begin
  Check(not Self.V.HasBranch, 'No branch should be assigned on creation');

  Self.V.Branch := BranchMagicCookie + 'f00';
  Check(Self.V.HasBranch, 'Branch should have been set');
end;

procedure TestTIdSipViaHeader.TestHasMaddr;
begin
  Check(not Self.V.HasMaddr, 'No maddr should be assigned on creation');

  Self.V.Maddr := '127.0.0.1';
  Check(Self.V.HasMaddr, 'Maddr should have been set');
end;

procedure TestTIdSipViaHeader.TestHasReceived;
begin
  Check(not Self.V.HasReceived, 'No Received should be assigned on creation');

  Self.V.Received := '127.0.0.1';
  Check(Self.V.HasReceived, 'Received should have been set');
end;

procedure TestTIdSipViaHeader.TestHasRport;
begin
  Check(not Self.V.HasRport, 'No rport should be assigned on creation');

  Self.V.Rport := 1024;
  Check(Self.V.HasRport, 'Rport should have been set');
end;

procedure TestTIdSipViaHeader.TestIsDefaultPortForTransport;
var
  AllTransports: TStrings;
  I:             Integer;
  Port:          Cardinal;
  Transport:     String;
begin
  AllTransports := TStringList.Create;
  try
    TIdSipTransportRegistry.SecureTransports(AllTransports);
    TIdSipTransportRegistry.InsecureTransports(AllTransports);

    for I := 0 to AllTransports.Count - 1 do begin
      Transport := AllTransports[I];
      Port      := TIdSipTransportRegistry.DefaultPortFor(Transport);

      Check(Self.V.IsDefaultPortForTransport(Port, Transport),
            'Default port for ' + Transport);
      Check(not Self.V.IsDefaultPortForTransport(Port + 1, Transport),
            'Default port + 1 for ' + Transport);
    end;
  finally
    AllTransports.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestIsRFC3261Branch;
begin
  Self.V.Value := 'SIP/2.0/TCP gw1.leo-ix.org';
  Check(not Self.V.IsRFC3261Branch, 'No branch');

  Self.V.Branch := 'z9hG4b';
  Check(not Self.V.IsRFC3261Branch, 'z9hG4b');

  Self.V.Branch := BranchMagicCookie;
  Check(Self.V.IsRFC3261Branch, 'RFC 3261 magic cookie');

  Self.V.Branch := BranchMagicCookie + '1234';
  Check(Self.V.IsRFC3261Branch, 'RFC 3261 magic cookie + ''1234''');
end;

procedure TestTIdSipViaHeader.TestEquals;
var
  Hop2: TIdSipViaHeader;
begin
  Hop2 := TIdSipViaHeader.Create;
  try
    Self.V.SentBy     := '127.0.0.1';
    Self.V.Port       := 5060;
    Self.V.SipVersion := 'SIP/2.0';
    Self.V.Transport  := SctpTransport;

    Hop2.SentBy     := '127.0.0.1';
    Hop2.Port       := 5060;
    Hop2.SipVersion := 'SIP/2.0';
    Hop2.Transport  := SctpTransport;

    Check(V.Equals(Hop2), 'V.Equals(Hop2)');
    Check(Hop2.Equals(V), 'Hop2.Equals(V)');

    Self.V.SentBy := '127.0.0.2';
    Check(not Self.V.Equals(Hop2), 'V.Equals(Hop2); Host');
    Check(not Hop2.Equals(Self.V), 'Hop2.Equals(V); Host');
    Self.V.SentBy := '127.0.0.1';
    Check(Self.V.Equals(Hop2), 'V.Equals(Hop2); Host reset');
    Check(Hop2.Equals(Self.V), 'Hop2.Equals(V); Host reset');

    Self.V.Port := 111;
    Check(not Self.V.Equals(Hop2), 'V.Equals(Hop2); Port');
    Check(not Hop2.Equals(Self.V), 'Hop2.Equals(V); Port');
    Self.V.Port := 5060;
    Check(Self.V.Equals(Hop2), 'V.Equals(Hop2); Port reset');
    Check(Hop2.Equals(Self.V), 'Hop2.Equals(V); Port reset');

    Self.V.SipVersion := 'xxx';
    Check(not Self.V.Equals(Hop2), 'V.Equals(Hop2); SipVersion');
    Check(not Hop2.Equals(Self.V), 'Hop2.Equals(V); SipVersion');
    Self.V.SipVersion := 'SIP/2.0';
    Check(Self.V.Equals(Hop2), 'V.Equals(Hop2); SipVersion reset');
    Check(Hop2.Equals(Self.V), 'Hop2.Equals(V); SipVersion reset');

    Self.V.Transport := TcpTransport;
    Check(not Self.V.Equals(Hop2), 'V.Equals(Hop2); Transport');
    Check(not Hop2.Equals(Self.V), 'Hop2.Equals(V); Transport');
  finally
    Hop2.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestEqualsBranchIsCaseInsensitive;
var
  Via: TIdSipViaHeader;
begin
  Via := TIdSipViaHeader.Create;
  try
    Self.V.Value := 'SIP/2.0/TCP gw1.leo-ix.net;branch=z9hG4bKdecafbad';
    Via.Value := Self.V.Value + Self.V.ParamsAsString;

    Via.Branch := Uppercase(Via.Branch);

    Check(Self.V.Equals(Via), 'V = Via');
    Check(Via.Equals(Self.V), 'Via = V');
  finally
    Via.Free;
  end;
end;

procedure TestTIdSipViaHeader.TestMaddr;
begin
  Self.V.Maddr := '1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Maddr, 'IPv4 address');

  Self.V.Maddr := '[fe80::201:2ff:fef0]';
  CheckEquals('[fe80::201:2ff:fef0]', Self.V.Maddr, 'IPv6 reference');

  Self.V.Maddr := 'one.two.three.four';
  CheckEquals('one.two.three.four', Self.V.Maddr, 'FQDN');

  try
    Self.V.Maddr := 'fe80::201:2ff:fef0';
    Fail('Failed to bail out on IPv6 address');
  except
    on EBadHeader do;
  end;
end;

procedure TestTIdSipViaHeader.TestName;
begin
  CheckEquals(ViaHeaderFull, Self.V.Name, 'Name');

  Self.V.Name := 'foo';
  CheckEquals(ViaHeaderFull, Self.V.Name, 'Name after set');
end;

procedure TestTIdSipViaHeader.TestReceived;
begin
  Self.V.Received := '1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Received, 'IPv4 address');

  Self.V.Received := 'fe80::201:2ff:fef0';
  CheckEquals('fe80::201:2ff:fef0', Self.V.Received, 'IPv6 address');

  try
    Self.V.Received := 'one.two.three.four';
    Fail('Failed to bail out on FQDN');
  except
    on EBadHeader do;
  end;

  try
    Self.V.Received := 'fjksahflkh fasdf';
    Fail('Failed to bail out on nonsense');
  except
    on EBadHeader do;
  end;

  Self.V.Received := '[fe80::201:2ff:fef0]';
  CheckEquals('fe80::201:2ff:fef0', Self.V.Received, 'IPv6 reference with stripped []s');  
end;

procedure TestTIdSipViaHeader.TestRemoveBranch;
const
  BaseVia = 'SIP/1.5/UDP 127.0.0.1';
begin
  Self.V.Value := BaseVia + ';branch=' + BranchMagicCookie + 'heehee';
  Self.V.RemoveBranch;

  CheckEquals(BaseVia,
              Self.V.FullValue,
              'AsString after RemoveBranch');
  Check(not Self.V.HasBranch,
        'HasBranch claims there''s still a branch');
end;

procedure TestTIdSipViaHeader.TestRoutingAddress;
const
  Domain   = 'gw1.leo-ix.net';
  Maddr    = 'bcast.earth.net';
  Received = '::dead:beef';
begin
  Self.V.Value := 'SIP/2.0/UDP ' + Domain;
  CheckEquals(Domain, Self.V.RoutingAddress, 'Just a domain');

  Self.V.Received := Received;
  CheckEquals(Received, Self.V.RoutingAddress, 'Received must override sent-by');

  Self.V.Maddr := Maddr;
  CheckEquals(Maddr, Self.V.RoutingAddress, 'Maddr must override everything');
end;

procedure TestTIdSipViaHeader.TestRoutingPort;
const
  Port  = 5060;
  RPort = 6666;
begin
  Self.V.Value := 'SIP/2.0/UDP gw1.leo-ix.net';
  CheckEquals(TIdSipTransportRegistry.DefaultPortFor(Self.V.Transport),
              Self.V.RoutingPort,
              'Implicit port');

  Self.V.Port := Port;
  CheckEquals(Port, Self.V.RoutingPort, 'Explicit port');

  Self.V.RPort := RPort;
  CheckEquals(RPort, Self.V.RoutingPort, 'Rport must override port');
end;

procedure TestTIdSipViaHeader.TestSrvQuery;
const
  Domain = 'gw1.leo-ix.net';
begin
  Self.V.Value := 'SIP/2.0/TLS ' + Domain;
  CheckEquals('_sips._tcp.' + Domain, Self.V.SrvQuery, Self.V.Transport);

  Self.V.Transport := TcpTransport;
  CheckEquals('_sip._tcp.' + Domain, Self.V.SrvQuery, Self.V.Transport);

  Self.V.Transport := UdpTransport;
  CheckEquals('_sip._udp.' + Domain, Self.V.SrvQuery, Self.V.Transport);

  Self.V.Transport := SctpTransport;
  CheckEquals('_sip._sctp.' + Domain, Self.V.SrvQuery, Self.V.Transport);

  Self.V.Transport := TlsOverSctpTransport;
  CheckEquals('_sips._sctp.' + Domain, Self.V.SrvQuery, Self.V.Transport);
end;

procedure TestTIdSipViaHeader.TestTTL;
begin
  Self.V.TTL := 0;
  CheckEquals(0, V.TTL, 'TTL set to 0');

  Self.V.TTL := 255;
  CheckEquals(255, V.TTL, 'TTL set to 255');
end;

procedure TestTIdSipViaHeader.TestValue;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('127.0.0.1',   Self.V.SentBy,         '1: SentBy');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '1: Parameters');
  CheckEquals(TIdSipTransportRegistry.DefaultPortFor(Self.V.Transport),
              Self.V.Port,
              '1: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '1: SipVersion');
  CheckEquals(UdpTransport,  Self.V.Transport,      '1: Transport');
  CheckEquals('SIP/1.5/UDP 127.0.0.1',
              Self.V.Value,
              'Port added?');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '2: SentBy');
  CheckEquals('',              Self.V.ParamsAsString, '2: Parameters');
  CheckEquals(TIdSipTransportRegistry.DefaultPortFor(Self.V.Transport),
              Self.V.Port,
              '2: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '2: SipVersion');
  CheckEquals(TlsTransport,    Self.V.Transport,      '2: Transport');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1:666;tag=heehee';
  CheckEquals('127.0.0.1',   Self.V.SentBy,         '3: SentBy');
  CheckEquals(';tag=heehee', Self.V.ParamsAsString, '3: Parameters');
  CheckEquals(666,           Self.V.Port,           '3: Port');
  CheckEquals('SIP/1.5',     Self.V.SipVersion,     '3: SipVersion');
  CheckEquals(UdpTransport,  Self.V.Transport,      '3: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666;haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '4: SentBy');
  CheckEquals(';haha=heehee',  Self.V.ParamsAsString, '4: Parameters');
  CheckEquals(666,             Self.V.Port,           '4: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '4: SipVersion');
  CheckEquals(TlsTransport,    Self.V.Transport,      '4: Transport');

  Self.V.Value := 'SIP/1.5/TLS 127.0.0.1:666 '#13#10' ; haha=heehee';
  CheckEquals('127.0.0.1',     Self.V.SentBy,         '5: SentBy');
  CheckEquals(';haha=heehee',  Self.V.ParamsAsString, '5: Parameters');
  CheckEquals(666,             Self.V.Port,           '5: Port');
  CheckEquals('SIP/1.5',       Self.V.SipVersion,     '5: SipVersion');
  CheckEquals(TlsTransport,    Self.V.Transport,      '5: Transport');
end;

procedure TestTIdSipViaHeader.TestValueTorture;
begin
  Self.V.Value := 'Via: SIP/2.0/UDP 135.180.130.133;;,;';
  Check(Self.V.IsMalformed,
        'Mad Via syntax doesn''t show up as invalid');
end;

procedure TestTIdSipViaHeader.TestValueWithBranch;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Branch, 'No Branch parameter');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Branch, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=' + BranchMagicCookie + '1234';
  CheckEquals(BranchMagicCookie + '1234', Self.V.Branch, BranchMagicCookie + '1234');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=www.google.com';
  CheckEquals('www.google.com', Self.V.Branch, 'FQDN');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=';
  Check(Self.V.IsMalformed,
        'Failed to bail out with empty string');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;branch=one two';
  Check(Self.V.IsMalformed,
        'Failed to bail out with multiple tokens');
end;

procedure TestTIdSipViaHeader.TestValueWithDefaultPort;
const
  ValueWithPort = 'SIP/2.0/UDP 127.0.0.1:5060';
begin
  Self.V.Value := ValueWithPort;
  CheckEquals(ValueWithPort, Self.V.Value, 'Port lost');
end;

procedure TestTIdSipViaHeader.TestValueWithIPv6NumericAddress;
const
  IPv6 = '[2002:DEAD:BEEF:1::1]';
begin
  Self.V.Value := 'SIP/2.0/UDP ' + IPv6;

  CheckEquals(IPv6, Self.V.SentBy, 'IPv6 sent-by');
end;

procedure TestTIdSipViaHeader.TestValueWithIPv6NumericAddressAndPort;
const
  IPv6 = '[2002:DEAD:BEEF:1::1]';
  Port = 6000;
var
  Value: String;
begin
  Value := 'SIP/2.0/UDP ' + IPv6 + ':' + IntToStr(Port);
  Self.V.Value := Value;

  CheckEquals(IPv6, Self.V.SentBy, 'IPv6 sent-by');
  CheckEquals(Port, Self.V.Port, 'Port');

  CheckEquals(Value, Self.V.Value, 'Port lost');
end;

procedure TestTIdSipViaHeader.TestValueWithSettingDefaultPort;
const
  ValueWithPort = 'SIP/2.0/UDP 127.0.0.1:5060';
begin
  Self.V.Value := 'SIP/2.0/UDP 127.0.0.1';

  Self.V.Port := TIdSipTransportRegistry.DefaultPortFor(Self.V.Transport);
  CheckEquals(ValueWithPort, Self.V.Value, 'Port lost');
end;

procedure TestTIdSipViaHeader.TestValueWithMaddr;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Maddr, 'No maddr parameter');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=1.2.3.4';
  CheckEquals('1.2.3.4', Self.V.Maddr, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=[fe80::201:2ff:fef0]';
  CheckEquals('[fe80::201:2ff:fef0]', Self.V.Maddr, 'IPv6 reference');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=www.google.com';
  CheckEquals('www.google.com', Self.V.Maddr, 'FQDN');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;maddr=fe80::201:2ff:fef0';
  Check(Self.V.IsMalformed,'Failed to bail out with IPv6 address');
end;

procedure TestTIdSipViaHeader.TestValueWithReceived;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals('', Self.V.Received,  'Received value when not present');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=4.3.2.1';
  CheckEquals('4.3.2.1', Self.V.Received, 'IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=fe80::201:2ff:fef0';
  CheckEquals('fe80::201:2ff:fef0', Self.V.Received, 'IPv6 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=';
  Check(Self.V.IsMalformed,
        'Failed to bail out with empty string');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=www.google.com';
  Check(Self.V.IsMalformed,
        'Failed to bail out with FQDN');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=256.0.0.0';
  Check(Self.V.IsMalformed,
        'Failed to bail out with malformed IPv4 address');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;received=skjfhdlskfhsdfshdfhs';
  Check(Self.V.IsMalformed,
        'Failed to bail out with nonsense');
end;

procedure TestTIdSipViaHeader.TestValueWithTTL;
begin
  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;tag=heehee';
  CheckEquals(0, Self.V.TTL, 'TTL value when not present');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=255';
  CheckEquals(255, Self.V.TTL, 'TTL of 255');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=0';
  CheckEquals(0, Self.V.TTL, 'TTL of 0');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=256';
  Check(Self.V.IsMalformed,
        'Failed to bail on invalid TTL (''256'') via SetValue');

  Self.V.Value := 'SIP/1.5/UDP 127.0.0.1;ttl=a';
  Check(Self.V.IsMalformed,
        'Failed to bail on invalid TTL (''a'') via SetValue');
end;

//******************************************************************************
//* TestTIdSipWarningHeader                                                    *
//******************************************************************************
//* TestTIdSipWarningHeader Public methods *************************************

procedure TestTIdSipWarningHeader.SetUp;
begin
  inherited SetUp;

  Self.W := Self.Header as TIdSipWarningHeader;
end;

//* TestTIdSipWarningHeader Protected methods **********************************

function TestTIdSipWarningHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipWarningHeader;
end;

//* TestTIdSipWarningHeader Published methods **********************************

procedure TestTIdSipWarningHeader.TestGetValue;
begin
  Self.W.Code := 302;
  Self.W.Agent := 'gw1.leo-ix.net';
  Self.W.Text := 'Case is not home';

  CheckEquals('302 gw1.leo-ix.net "Case is not home"',
              Self.W.Value,
              'Value');
end;

procedure TestTIdSipWarningHeader.TestName;
begin
  CheckEquals(WarningHeader, Self.W.Name, 'Name');

  Self.W.Name := 'foo';
  CheckEquals(WarningHeader, Self.W.Name, 'Name after set');
end;

procedure TestTIdSipWarningHeader.TestValue;
begin
  Self.W.Value := '301 wsfrank "I dont know what message goes here"';
  CheckEquals(301,       Self.W.Code, 'Code');
  CheckEquals('wsfrank', Self.W.Agent, 'Agent');

  CheckEquals('I dont know what message goes here',
              Self.W.Text,
              'Text');
end;

procedure TestTIdSipWarningHeader.TestSetValueIPv6;
begin
  Self.W.Value := '301 [1::127.0.0.1] "Not much"';

  CheckEquals(301,              Self.W.Code,  'Code');
  CheckEquals('[1::127.0.0.1]', Self.W.Agent, 'Agent');
  CheckEquals('Not much',       Self.W.Text,  'Text');
end;

procedure TestTIdSipWarningHeader.TestSetValueMalformed;
begin
  Self.W.Value := 'a bad "header"';
  Check(Self.W.IsMalformed,
        'Failed to bail on a non-numeric warn-code');

  Self.W.Value := '22 a "bad header"';
  Check(Self.W.IsMalformed,
        'Failed to bail on a too-short warn-code');

  Self.W.Value := '2222 a "bad header"';
  Check(Self.W.IsMalformed,
        'Failed to bail on a too-long warn-code');

  Self.W.Value := '301 a bad header';
  Check(Self.W.IsMalformed,
        'Failed to bail on an unquoted warn-text');

  Self.W.Value := '302 a "bad header';
  Check(Self.W.IsMalformed,
        'Failed to bail on a malquoted warn-text');

  Self.W.Value := '301 a "bad header";tag=xyz';
  Check(Self.W.IsMalformed,
        'Failed to bail on a malquoted warn-text (parameters)');

  Self.W.Value := '301 gw1.leo-ix.net:ababa Case is not home';
  Check(Self.W.IsMalformed,
        'Failed to bail on a malquoted hostport');
end;

procedure TestTIdSipWarningHeader.TestSetValuePortSpecified;
begin
  Self.W.Value := '302 gw1.leo-ix.net:5060 "Case is not home"';

  CheckEquals(302,
              Self.W.Code,
              'Code');
  CheckEquals('gw1.leo-ix.net:5060',
              Self.W.Agent,
              'Agent');
  CheckEquals('Case is not home',
              Self.W.Text,
              'Text');
end;

procedure TestTIdSipWarningHeader.TestSetValueToken;
const
  WeirdToken = '%%%.this-is_a~token!';
begin
  Self.W.Value := '302 ' + WeirdToken + ' "Case is not home"';

  CheckEquals(302,
              Self.W.Code,
              'Code');
  CheckEquals(WeirdToken,
              Self.W.Agent,
              'Agent');
  CheckEquals('Case is not home',
              Self.W.Text,
              'Text');
end;

//******************************************************************************
//* TestTIdSipWeightedCommaSeparatedHeader                                     *
//******************************************************************************
//* TestTIdSipWeightedCommaSeparatedHeader Public methods **********************

procedure TestTIdSipWeightedCommaSeparatedHeader.SetUp;
begin
  inherited SetUp;

  Self.W := Self.Header as TIdSipWeightedCommaSeparatedHeader;
end;

//* TestTIdSipWeightedCommaSeparatedHeader Published methods *******************

function TestTIdSipWeightedCommaSeparatedHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipWeightedCommaSeparatedHeader;
end;

//* TestTIdSipWeightedCommaSeparatedHeader Published methods *******************

procedure TestTIdSipWeightedCommaSeparatedHeader.TestAddValue;
begin
  CheckEquals(0, Self.W.ValueCount, 'Empty string');

  Self.W.AddValue('text/plain');
  CheckEquals(1,            Self.W.ValueCount, 'One Add');
  CheckEquals('text/plain', Self.W.Values[0].Value,  '[0].Value');
  CheckEquals(1000,         Self.W.Values[0].Weight, '[0].Weight');

  Self.W.AddValue('text/xml', 700);
  CheckEquals(2,          Self.W.ValueCount, 'Two Adds');
  CheckEquals('text/xml', Self.W.Values[1].Value,  '[1].Value');
  CheckEquals(700,        Self.W.Values[1].Weight, '[1].Weight');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestClearValues;
begin
  Self.W.AddValue('text/plain');
  Self.W.AddValue('text/plain');
  Self.W.AddValue('text/plain');

  Self.W.ClearValues;

  CheckEquals(0, Self.W.ValueCount, 'ClearValues didn''t');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestGetValue;
begin
  Self.W.AddValue('text/plain', 700);
  Self.W.Values[0].Parameters.Values['foo'] := 'bar';
  Self.W.AddValue('text/t140');

  CheckEquals('text/plain;q=0.7;foo=bar, text/t140', Self.W.Value, 'GetValue');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestValue;
begin
  Self.W.Value := '';
  CheckEquals(0, Self.W.ValueCount, 'Empty string');

  Self.W.Value := 'text/plain';
  CheckEquals(1,            Self.W.ValueCount,                 '1: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '1: Parameter count');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '1: Value');
  CheckEquals(1000,         Self.W.Values[0].Weight,           '1: Weight');

  Self.W.Value := 'text/plain;q=0.7';
  CheckEquals(1,            Self.W.ValueCount,                 '2: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '2: Parameter count');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '2: Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '2: Weight');

  Self.W.Value := 'text/plain;q=0.7;foo=bar';
  CheckEquals(1,            Self.W.ValueCount,                 '3: Count');
  CheckEquals(1,            Self.W.Values[0].Parameters.Count, '3: Parameter count');
  Check(Self.W.Values[0].Parameters.HasParameter('foo'),       '3: Parameter foo not added');
  CheckEquals('bar',
              Self.W.Values[0].Parameters.ParamValue('foo'),   '3: Parameter foo has incorrect value');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '3: Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '3: Weight');

  Self.W.Value := 'text/plain;q=0.7;foo=bar, text/t140';
  CheckEquals(2,            Self.W.ValueCount,                 '4: Count');
  CheckEquals(1,            Self.W.Values[0].Parameters.Count, '4: [0].Parameter count');

  Check(Self.W.Values[0].Parameters.HasParameter('foo'),       '4: [0] Parameter foo not added');
  CheckEquals('bar',
              Self.W.Values[0].Parameters.ParamValue('foo'),   '4: [0] Parameter foo has incorrect value');
  CheckEquals('text/plain', Self.W.Values[0].Value,            '4: [0].Value');
  CheckEquals(700,          Self.W.Values[0].Weight,           '4: [0].Weight');

  CheckEquals(0,            Self.W.Values[1].Parameters.Count, '4: [1].Parameter count');
  CheckEquals('text/t140',  Self.W.Values[1].Value,            '4: [1].Value');
  CheckEquals(1000,         Self.W.Values[1].Weight,           '4: [1].Weight');

  Self.W.Value := 'text/plain;q=0.0';
  CheckEquals(1,            Self.W.ValueCount,                 '5: Count');
  CheckEquals(0,            Self.W.Values[0].Parameters.Count, '5: Parameter count');
  CheckEquals(0,            Self.W.Values[0].Weight,           '5: Weight');
end;

procedure TestTIdSipWeightedCommaSeparatedHeader.TestValueMalformed;
begin
  Self.W.Value := 'text/plain;q=1.7';
  Check(Self.W.IsMalformed,
        'Failed to bail out on malformed qvalue (q > 1)');

  Self.W.Value := 'text/plain;q=a';
  Check(Self.W.IsMalformed,
        'Failed to bail out on malformed qvalue (q not numeric)');
end;

//******************************************************************************
//* TestTIdSipWWWAuthenticateHeader                                            *
//******************************************************************************
//* TestTIdSipWWWAuthenticateHeader Public methods *****************************

procedure TestTIdSipWWWAuthenticateHeader.SetUp;
begin
  inherited SetUp;

  Self.W := Self.Header as TIdSipWWWAuthenticateHeader;
end;

procedure TestTIdSipWWWAuthenticateHeader.TestCredentialHeaderType;
var
  Credential: TIdSipAuthorizationHeader;
begin
  Credential := Self.W.CredentialHeaderType.Create;
  try
    CheckEquals(AuthorizationHeader,
                Credential.Name,
                Self.W.Name + ' credentials header');
  finally
    Credential.Free;
  end;
end;

procedure TestTIdSipWWWAuthenticateHeader.TestName;
begin
  CheckEquals(WWWAuthenticateHeader,
              Self.W.Name,
              'Name');
end;

//* TestTIdSipWWWAuthenticateHeader Protected methods **************************

function TestTIdSipWWWAuthenticateHeader.HeaderType: TIdSipHeaderClass;
begin
  Result := TIdSipWWWAuthenticateHeader;
end;

//******************************************************************************
//* TTestHeadersList                                                           *
//******************************************************************************
//* TTestHeadersList Public methods ********************************************

procedure TTestHeadersList.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;
end;

procedure TTestHeadersList.TearDown;
begin
  Self.Headers.Free;

  inherited TearDown;
end;

//* TTestHeadersList Published methods *****************************************

procedure TTestHeadersList.TestAddInReverseOrder;
begin
  Fail(Self.ClassName + ' must override TestAddInReverseOrder');
end;

//******************************************************************************
//* TestTIdSipHeadersFilter                                                    *
//******************************************************************************
//* TestTIdSipHeadersFilter Public methods *************************************

procedure TestTIdSipHeadersFilter.SetUp;
begin
  inherited SetUp;

  Self.Headers.Add(MaxForwardsHeader).Value       := '70';
  Self.Headers.Add(RouteHeader).Value             := 'wsfrank <sip:192.168.1.43>';
  Self.Headers.Add(ContentLengthHeaderFull).Value := '29';
  Self.Headers.Add(RouteHeader).Value             := 'localhost <sip:127.0.0.1>';

  Self.Filter := TIdSipHeadersFilter.Create(Self.Headers, RouteHeader);
end;

procedure TestTIdSipHeadersFilter.TearDown;
begin
  Self.Filter.Free;

  inherited TearDown;
end;

//* TestTIdSipHeadersFilter Published methods **********************************

procedure TestTIdSipHeadersFilter.TestAdd;
var
  Route: TIdSipRouteHeader;
  OriginalCount: Cardinal;
begin
  OriginalCount := Self.Filter.Count;

  Route := TIdSipRouteHeader.Create;
  try
    Route.Value := '<sip:127.0.0.1>';

    Self.Filter.Add(Route);
    CheckEquals(OriginalCount + 1, Self.Filter.Count, 'Count after Add');
  finally
    Route.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestAddInReverseOrder;
var
  NewHeaders: TIdSipHeaders;
  Filter:     TIdSipHeadersFilter;
begin
  Self.Headers.Clear;

  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLengthHeaderFull).Value := '22';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.1>';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.2>';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.3>';

    Filter := TIdSipHeadersFilter.Create(NewHeaders, RouteHeader);
    try
      CheckEquals(0, Self.Headers.Count, 'Count before Add(Filter)');

      Self.Headers.AddInReverseOrder(Filter);
      CheckEquals(Filter.Count, Self.Headers.Count, 'Count after Add(Filter)');

      CheckEquals('<sip:127.0.0.3>', Self.Headers.Items[0].Value, '1st header');
      CheckEquals('<sip:127.0.0.2>', Self.Headers.Items[1].Value, '2nd header');
      CheckEquals('<sip:127.0.0.1>', Self.Headers.Items[2].Value, '3rd header');
    finally
      Filter.Free;
    end;
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestCount;
begin
  CheckEquals(2, Self.Filter.Count, 'Count with two headers');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';
  CheckEquals(4, Self.Filter.Count, 'Count with newly added headers');
end;

procedure TestTIdSipHeadersFilter.TestFirst;
begin
  Self.Headers.Clear;
  Self.Headers.First;
  CheckNull(Self.Filter.CurrentHeader,
            'First element of an empty collection');

  Self.Headers.Add(Self.Filter.HeaderName);
  Self.Headers.First;
  Self.Filter.First;
  Check(Self.Headers.CurrentHeader = Self.Filter.CurrentHeader,
  'First element of a non-empty collection');
end;

procedure TestTIdSipHeadersFilter.TestIsEmpty;
begin
  Check(not Self.Filter.IsEmpty, 'IsEmpty with 2 Route headers');
  Self.Headers.Remove(Self.Headers[RouteHeader]);
  Self.Headers.Remove(Self.Headers[RouteHeader]);
  Check(Self.Filter.IsEmpty, 'IsEmpty with no Route headers');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Check(not Self.Filter.IsEmpty, 'IsEmpty after Headers.Add(RouteHeader)');

  Self.Headers.Clear;
  Check(Self.Filter.IsEmpty, 'IsEmpty after Headers.Clear');
end;

procedure TestTIdSipHeadersFilter.TestEqualsFilter;
var
  H: TIdSipHeaders;
  F: TIdSipHeadersFilter;
begin
  Self.Filter.RemoveAll;

  H := TIdSipHeaders.Create;
  try
    F := TIdSipHeadersFilter.Create(H, RouteHeader);
    try
      Check(Self.Filter.Equals(F), 'Empty path = Empty path');

      H.Add(F.HeaderName).Value := '<sip:127.0.0.1:1>';

      Check(not Self.Filter.Equals(F), 'Empty path <> non-empty path');

      H.Add(RouteHeader).Value := '<sip:127.0.0.1:2>';

      Self.Filter.Add(F);
      Check(Self.Filter.Equals(F), 'Identical paths');

      Self.Filter.Items[Self.Filter.Count - 1].Value := '<sip:127.0.0.1:1>';
      Check(not Self.Filter.Equals(F), 'Last header differs');
    finally
      F.Free;
    end;
  finally
    H.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestEqualsHeaders;
var
  H: TIdSipHeaders;
begin
  Self.Filter.RemoveAll;

  H := TIdSipHeaders.Create;
  try
    Check(Self.Filter.Equals(H), 'Empty set = Empty set');

    H.Add(RouteHeader).Value := '<sip:127.0.0.1:1>';

    Check(not Self.Filter.Equals(H), 'Empty set <> non-empty set');

    H.Add(RouteHeader).Value := '<sip:127.0.0.1:2>';

    Self.Filter.Add(H);
    Check(Self.Filter.Equals(H), 'Identical sets');

    Self.Filter.Items[Self.Filter.Count - 1].Value := '<sip:127.0.0.1:1>';
    Check(not Self.Filter.Equals(H), 'Last header differs');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestEqualsOrderIrrelevant;
var
  H: TIdSipHeaders;
begin
  Self.Filter.RemoveAll;

  H := TIdSipHeaders.Create;
  try
    Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1:1>';
    Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.1:2>';

    H.AddInReverseOrder(Self.Filter);

    Check(Self.Filter.Equals(H),
          'Identical sets but in reverse order');
  finally
    H.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestItems;
begin
  CheckEquals('wsfrank <sip:192.168.1.43>', Self.Filter.Items[0].Value, '1st Route');
  CheckEquals('localhost <sip:127.0.0.1>',  Self.Filter.Items[1].Value, '2nd Route');

  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';
  CheckEquals('<sip:127.0.0.2>',  Self.Filter.Items[2].Value, '3rd Route');
  CheckEquals('<sip:127.0.0.3>',  Self.Filter.Items[3].Value, '4h Route');
end;

procedure TestTIdSipHeadersFilter.TestIteratorVisitsAllHeaders;
var
  Expected: TIdSipHeaders;
begin
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.3>';

  Self.Filter.First;
  while Self.Filter.HasNext do begin
    Self.Filter.CurrentHeader.Params['foo'] := 'bar';
    Self.Filter.Next;
  end;

  Expected := TIdSipHeaders.Create;
  try
    Expected.Add(MaxForwardsHeader).Value := '70';
    Expected.Add(RouteHeader).Value := 'wsfrank <sip:192.168.1.43>;foo=bar';
    Expected.Add(ContentLengthHeaderFull).Value := '29';
    Expected.Add(RouteHeader).Value := 'localhost <sip:127.0.0.1>;foo=bar';
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.2>;foo=bar';
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.3>;foo=bar';
    Check(Expected.Equals(Self.Headers), 'Not all (Route) headers visited');
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestRemove;
var
  Route: TIdSipRouteHeader;
begin
  Route := TIdSipRouteHeader.Create;
  try
    Route.Value := '<sip:127.0.0.1>';
    CheckEquals(2, Self.Filter.Count, 'Count with two headers');

    Self.Filter.Add(Route);
    CheckEquals(3, Self.Filter.Count, 'Count after Add');

    Self.Filter.Remove(Self.Headers[Self.Filter.HeaderName]);
    CheckEquals(2, Self.Filter.Count, 'Count after Remove');

    Self.Filter.Remove(Self.Headers[ContentLengthHeaderFull]);
    CheckEquals(2, Self.Filter.Count, 'Count after Remove''ing a non-Route header');
  finally
    Route.Free;
  end;
end;

procedure TestTIdSipHeadersFilter.TestRemoveAll;
begin
  Self.Filter.RemoveAll;
  CheckEquals(0, Self.Filter.Count, 'Route headers not removed');
end;

//******************************************************************************
//* TestTIdSipHeaders                                                          *
//******************************************************************************
//* TestTIdSipHeaders Private methods ******************************************

procedure TestTIdSipHeaders.CheckType(ExpectedClassType: TClass;
                                      ReceivedObject: TObject;
                                      Message: String = '');
begin
  Self.CheckEquals(ExpectedClassType.ClassName,
                   ReceivedObject.ClassName,
                   Message);
end;

//* TestTIdSipHeaders Published methods ****************************************

procedure TestTIdSipHeaders.TestAddAndCount;
begin
  CheckEquals(0, Self.Headers.Count, 'Supposedly an empty set of headers');
  CheckEquals(TIdSipHeader.ClassName,
              Self.Headers.Add(OrganizationHeader).ClassName,
              'Incorrect return type');
  CheckEquals(1, Self.Headers.Count, 'Failed to add new header');
end;

procedure TestTIdSipHeaders.TestAddHeader;
var
  NewHeader: TIdSipHeader;
begin
  NewHeader := TIdSipHeader.Create;
  try
    NewHeader.Name  := 'X-Header';
    NewHeader.Value := 'boogaloo;foo=bar';

    CheckEquals(0, Self.Headers.Count, 'Count before Add(Header)');
    Self.Headers.Add(NewHeader);
    CheckEquals(1, Self.Headers.Count, 'Count after Add(Header)');

    Self.Headers.First;
    CheckEquals(NewHeader.AsString,
                Self.Headers.CurrentHeader.AsString,
                'Data not copied');
  finally
    NewHeader.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddHeaderName;
begin
  CheckEquals(0, Self.Headers.Count, 'Count before Add(HeaderName)');
  Self.Headers.Add('NewHeader').Value := 'FooBar';
  CheckEquals(1, Self.Headers.Count, 'Count after Add(HeaderName)');

  Self.Headers.First;
  CheckEquals('NewHeader: FooBar',
              Self.Headers.CurrentHeader.AsString,
              'AsString');
end;

procedure TestTIdSipHeaders.TestAddHeaders;
var
  NewHeaders: TIdSipHeaders;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';

    CheckEquals(0, Self.Headers.Count, 'Count before Add(Headers)');
    Self.Headers.Add(NewHeaders);
    CheckEquals(NewHeaders.Count, Self.Headers.Count, 'Count after Add(Headers)');

    Check(Self.Headers.Equals(NewHeaders), 'Headers weren''t properly added');
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddHeadersEmptyList;
var
  NewHeaders: TIdSipHeaders;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    CheckEquals(0, Self.Headers.Count, 'Count before Add(Headers)');

    Self.Headers.Add(NewHeaders);
    CheckEquals(NewHeaders.Count, Self.Headers.Count, 'Count after Add(Headers)');

    Check(Self.Headers.Equals(NewHeaders), 'Headers weren''t properly added');
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddHeadersFilter;
var
  NewHeaders: TIdSipHeaders;
  Filter:     TIdSipHeadersFilter;
  Expected:   TIdSipHeaders;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLengthHeaderFull).Value := '22';
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.1>';

    Filter := TIdSipHeadersFilter.Create(NewHeaders, ContentLanguageHeader);
    try
      CheckEquals(0, Self.Headers.Count, 'Count before Add(Filter)');

      Self.Headers.Add(Filter);
      CheckEquals(Filter.Count, Self.Headers.Count, 'Count after Add(Filter)');

      Expected := TIdSipHeaders.Create;
      try
        Expected.Add(ContentLanguageHeader).Value := 'en';
        Expected.Add(ContentLanguageHeader).Value := 'es';
        Expected.Add(ContentLanguageHeader).Value := 'fr';

        Check(Self.Headers.Equals(Expected), 'Filter doesn''t filter properly');
      finally
        Expected.Free;
      end;
    finally
      Filter.Free;
    end;
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddInReverseOrder;
var
  NewHeaders: TIdSipHeaders;
  Filter:     TIdSipHeadersFilter;
begin
  NewHeaders := TIdSipHeaders.Create;
  try
    NewHeaders.Add(ContentLengthHeaderFull).Value := '22';
    NewHeaders.Add(ContentLanguageHeader).Value := 'en';
    NewHeaders.Add(ContentLanguageHeader).Value := 'es';
    NewHeaders.Add(ContentLanguageHeader).Value := 'fr';
    NewHeaders.Add(RouteHeader).Value := '<sip:127.0.0.1>';

    Filter := TIdSipHeadersFilter.Create(NewHeaders, ContentLanguageHeader);
    try
      CheckEquals(0, Self.Headers.Count, 'Count before Add(Filter)');

      Self.Headers.AddInReverseOrder(Filter);
      CheckEquals(Filter.Count, Self.Headers.Count, 'Count after Add(Filter)');

      CheckEquals('fr', Self.Headers.Items[0].Value, '1st header');
      CheckEquals('es', Self.Headers.Items[1].Value, '2nd header');
      CheckEquals('en', Self.Headers.Items[2].Value, '3rd header');
    finally
      Filter.Free;
    end;
  finally
    NewHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestAddResultTypes;
begin
  CheckType(TIdSipWeightedCommaSeparatedHeader, Self.Headers.Add(AcceptHeader),               AcceptHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(AcceptEncodingHeader),       AcceptEncodingHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(AcceptLanguageHeader),       AcceptLanguageHeader);
  CheckType(TIdSipUriHeader,                    Self.Headers.Add(AlertInfoHeader),            AlertInfoHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(AllowHeader),                AllowHeader);
  CheckType(TIdSipAllowEventsHeader,            Self.Headers.Add(AllowEventsHeaderFull),      AllowEventsHeaderFull);
  CheckType(TIdSipAllowEventsHeader,            Self.Headers.Add(AllowEventsHeaderShort),     AllowEventsHeaderShort);
  CheckType(TIdSipAuthenticationInfoHeader,     Self.Headers.Add(AuthenticationInfoHeader),   AuthenticationInfoHeader);
  CheckType(TIdSipAuthorizationHeader,          Self.Headers.Add(AuthorizationHeader),        AuthorizationHeader);
  CheckType(TIdSipCallIdHeader,                 Self.Headers.Add(CallIDHeaderFull),           CallIDHeaderFull);
  CheckType(TIdSipCallIdHeader,                 Self.Headers.Add(CallIDHeaderShort),          CallIDHeaderShort);
  CheckType(TIdSipHeader,                       Self.Headers.Add(CallInfoHeader),             CallInfoHeader);
  CheckType(TIdSipContactHeader,                Self.Headers.Add(ContactHeaderFull),          ContactHeaderFull);
  CheckType(TIdSipContactHeader,                Self.Headers.Add(ContactHeaderShort),         ContactHeaderShort);
  CheckType(TIdSipContentDispositionHeader,     Self.Headers.Add(ContentDispositionHeader),   ContentDispositionHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(ContentEncodingHeaderFull),  ContentEncodingHeaderFull);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(ContentEncodingHeaderShort), ContentEncodingHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(ContentLanguageHeader),      ContentLanguageHeader);
  CheckType(TIdSipNumericHeader,                Self.Headers.Add(ContentLengthHeaderFull),    ContentLengthHeaderFull);
  CheckType(TIdSipNumericHeader,                Self.Headers.Add(ContentLengthHeaderShort),   ContentLengthHeaderShort);
  CheckType(TIdSipHeader,                       Self.Headers.Add(ContentTypeHeaderFull),      ContentTypeHeaderFull);
  CheckType(TIdSipHeader,                       Self.Headers.Add(ContentTypeHeaderShort),     ContentTypeHeaderShort);
  CheckType(TIdSipCSeqHeader,                   Self.Headers.Add(CSeqHeader),                 CSeqHeader);
  CheckType(TIdSipDateHeader,                   Self.Headers.Add(DateHeader),                 DateHeader);
  CheckType(TIdSipEventHeader,                  Self.Headers.Add(EventHeaderFull),            EventHeaderFull);
  CheckType(TIdSipEventHeader,                  Self.Headers.Add(EventHeaderShort),           EventHeaderShort);
  CheckType(TIdSipUriHeader,                    Self.Headers.Add(ErrorInfoHeader),            ErrorInfoHeader);
  CheckType(TIdSipNumericHeader,                Self.Headers.Add(ExpiresHeader),              ExpiresHeader);
  CheckType(TIdSipFromHeader,                   Self.Headers.Add(FromHeaderFull),             FromHeaderFull);
  CheckType(TIdSipFromHeader,                   Self.Headers.Add(FromHeaderShort),            FromHeaderShort);
  CheckType(TIdSipCallIdHeader,                 Self.Headers.Add(InReplyToHeader),            InReplyToHeader);
  CheckType(TIdSipMaxForwardsHeader,            Self.Headers.Add(MaxForwardsHeader),          MaxForwardsHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(MIMEVersionHeader),          MIMEVersionHeader);
  CheckType(TIdSipNumericHeader,                Self.Headers.Add(MinExpiresHeader),           MinExpiresHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(OrganizationHeader),         OrganizationHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(PriorityHeader),             PriorityHeader);
  CheckType(TIdSipProxyAuthenticateHeader,      Self.Headers.Add(ProxyAuthenticateHeader),    ProxyAuthenticateHeader);
  CheckType(TIdSipProxyAuthorizationHeader,     Self.Headers.Add(ProxyAuthorizationHeader),   ProxyAuthorizationHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(ProxyRequireHeader),         ProxyRequireHeader);
  CheckType(TIdSipRecordRouteHeader,            Self.Headers.Add(RecordRouteHeader),          RecordRouteHeader);
  CheckType(TIdSipReferToHeader,                Self.Headers.Add(ReferToHeaderFull),          ReferToHeaderFull);
  CheckType(TIdSipReferToHeader,                Self.Headers.Add(ReferToHeaderShort),         ReferToHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(RequireHeader),              RequireHeader);
  CheckType(TIdSipReplacesHeader,               Self.Headers.Add(ReplacesHeader),             ReplacesHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(ReplyToHeader),              ReplyToHeader);
  CheckType(TIdSipRetryAfterHeader,             Self.Headers.Add(RetryAfterHeader),           RetryAfterHeader);
  CheckType(TIdSipRouteHeader,                  Self.Headers.Add(RouteHeader),                RouteHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(ServerHeader),               ServerHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(SubjectHeaderFull),          SubjectHeaderFull);
  CheckType(TIdSipHeader,                       Self.Headers.Add(SubjectHeaderShort),         SubjectHeaderShort);
  CheckType(TIdSipSubscriptionStateHeader,      Self.Headers.Add(SubscriptionStateHeader),    SubscriptionStateHeader);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(SupportedHeaderFull),        SupportedHeaderFull);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(SupportedHeaderShort),       SupportedHeaderShort);
  CheckType(TIdSipTargetDialogHeader,           Self.Headers.Add(TargetDialogHeader),         TargetDialogHeader);
  CheckType(TIdSipTimestampHeader,              Self.Headers.Add(TimestampHeader),            TimestampHeader);
  CheckType(TIdSipToHeader,                     Self.Headers.Add(ToHeaderFull),               ToHeaderFull);
  CheckType(TIdSipToHeader,                     Self.Headers.Add(ToHeaderShort),              ToHeaderShort);
  CheckType(TIdSipCommaSeparatedHeader,         Self.Headers.Add(UnsupportedHeader),          UnsupportedHeader);
  CheckType(TIdSipHeader,                       Self.Headers.Add(UserAgentHeader),            UserAgentHeader);
  CheckType(TIdSipViaHeader,                    Self.Headers.Add(ViaHeaderFull),              ViaHeaderFull);
  CheckType(TIdSipViaHeader,                    Self.Headers.Add(ViaHeaderShort),             ViaHeaderShort);
  CheckType(TIdSipWarningHeader,                Self.Headers.Add(WarningHeader),              WarningHeader);
  CheckType(TIdSipWWWAuthenticateHeader,        Self.Headers.Add(WWWAuthenticateHeader),      WWWAuthenticateHeader);
end;

procedure TestTIdSipHeaders.TestAsString;
begin
  CheckEquals('',
              Self.Headers.AsString,
              'AsString with zero headers');

  Self.Headers.Add('Content-Length').Value := '28';
  Self.Headers['Content-Length'].Params['bogus'] := 'true';

  CheckEquals('Content-Length: 28;bogus=true'#13#10,
              Self.Headers.AsString,
              'AsString with one header');

  Self.Headers.Add('Content-Type').Value := 'text/xml';
  Self.Headers['Content-Type'].Params['kallisti'] := 'eris';

  CheckEquals('Content-Length: 28;bogus=true'#13#10
            + 'Content-Type: text/xml;kallisti=eris'#13#10,
              Self.Headers.AsString,
              'AsString with two headers');
end;

procedure TestTIdSipHeaders.TestClear;
begin
  Self.Headers.Clear;
  CheckEquals(0, Self.Headers.Count, 'Count after Clearing an empty list');

  Self.Headers.Add('Content-Length');
  Self.Headers.Add('Via');
  Self.Headers.Clear;
  CheckEquals(0, Self.Headers.Count, 'Count after Clearing a non-empty list');
end;

procedure TestTIdSipHeaders.TestDelete;
begin
  Self.Headers.Add('foo');
  Self.Headers.Add('bar');
  Self.Headers.Add('baz');
  Self.Headers.Add('quaax');

  Self.Headers.Delete(1);
  CheckEquals(3, Self.Headers.Count, 'Count after 1st Delete');
  CheckEquals('foo',   Self.Headers.Items[0].Name, '1: 1st header');
  CheckEquals('baz',   Self.Headers.Items[1].Name, '1: 2nd header');
  CheckEquals('quaax', Self.Headers.Items[2].Name, '1: 3rd header');

  Self.Headers.Delete(2);
  CheckEquals(2, Self.Headers.Count, 'Count after 2nd Delete');
  CheckEquals('foo',   Self.Headers.Items[0].Name, '2: 1st header');
  CheckEquals('baz',   Self.Headers.Items[1].Name, '2: 2nd header');

  Self.Headers.Delete(0);
  CheckEquals(1, Self.Headers.Count, 'Count after 3rd Delete');
  CheckEquals('baz', Self.Headers.Items[0].Name, '3: 1st header');

  Self.Headers.Delete(0);
  CheckEquals(0, Self.Headers.Count, 'Count after 4th Delete');
end;

procedure TestTIdSipHeaders.TestEqualsPenultimateHeaderNotEqual;
var
  OtherHeaders: TIdSipHeaders;
begin
  // The trick here is to have a non-equal header before the final header.
  Self.Headers.Add('foo').Value   := 'foo';
  Self.Headers.Add('quaax').Value := 'quaax';

  OtherHeaders := TIdSipHeaders.Create;
  try
    OtherHeaders.Add(Self.Headers);
    Self.Headers.First;
    OtherHeaders.First;
    OtherHeaders.CurrentHeader.Value := Self.Headers.CurrentHeader.Value + 'X';

    Check(not Self.Headers.Equals(OtherHeaders),
          'Despite a differing first header, Self.Headers = OtherHeaders');
    Check(not OtherHeaders.Equals(Self.Headers),
          'Despite a differing first header, OtherHeaders = Self.Headers');
  finally
    OtherHeaders.Free;
  end;
end;

procedure TestTIdSipHeaders.TestFirst;
begin
  Self.Headers.First;
  CheckNull(Self.Headers.CurrentHeader, 'First element of an empty collection');

  Self.Headers.Add('foo');
  Self.Headers.First;
  CheckNotNull(Self.Headers.CurrentHeader,
               'First element of a non-empty collection');
  CheckEquals('foo',
              Self.Headers.CurrentHeader.Name,
              'Name of first element');
end;

procedure TestTIdSipHeaders.TestGetAllButFirst;
var
  Expected: TIdSipHeaders;
  Received: TIdSipHeaderList;
begin
  Expected := TIdSipHeaders.Create;
  try
    Received := Self.Headers.GetAllButFirst;
    try
      Check(Expected.Equals(Received),
            'Incorrect headers returned, empty list');
    finally
      Received.Free;
    end;


    Expected.Add(ContentTypeHeaderFull).Value   := 'text/plain';
    Expected.Add(MaxForwardsHeader).Value       := '70';

    Self.Headers.Add(ContentLengthHeaderFull).Value := '29';
    Self.Headers.Add(Expected);

    Received := Self.Headers.GetAllButFirst;
    try
      Check(Expected.Equals(Received),
            'Incorrect headers returned, nonempty list');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

procedure TestTIdSipHeaders.TestHasHeader;
begin
  Check(not Self.Headers.HasHeader(''), '''''');
  Check(not Self.Headers.HasHeader('Content-Length'), 'Content-Length');

  Self.Headers.Add('Content-Length');
  Check(Self.Headers.HasHeader('Content-Length'), 'Content-Length not added');
end;

procedure TestTIdSipHeaders.TestIsMalformed;
begin
  Check(not Self.Headers.IsMalformed,
        'Empty list vacuously has valid syntax');

  Self.Headers.Add(MaxForwardsHeader).Value := '10';
  Check(not Self.Headers.IsMalformed,
        'Valid Max-Forwards');

  Self.Headers.Add(ExpiresHeader).Value := 'a';
  Check(Self.Headers.IsMalformed,
        'Invalid Expires');

  Self.Headers[ExpiresHeader].Value := '10';
  Check(not Self.Headers.IsMalformed,
        'Expires header made valid');
end;

procedure TestTIdSipHeaders.TestHeaders;
var
  Header: TIdSipHeader;
begin
  CheckEquals(CallIdHeaderFull,
              Self.Headers.Headers[CallIdHeaderFull].Name,
              'Returned newly created header: Name');
  CheckEquals('',
              Self.Headers.Headers[CallIdHeaderFull].Value,
              'Returned newly created header: Value');
  CheckEquals(1, Self.Headers.Count, 'Newly created header wasn''t added though');

  Header := Self.Headers.Add('Via');
  Check(Header = Self.Headers.Headers['Via'],
        'Incorrect header returned');
end;

procedure TestTIdSipHeaders.TestItems;
begin
  try
    Self.Headers.Items[0];
    Fail('Failed to bail out accessing the 1st header in an empty collection');
  except
    on EListError do;
  end;

  Self.Headers.Add('Content-Length');
  CheckEquals('Content-Length', Self.Headers.Items[0].Name, 'Name of 1st header');
end;

procedure TestTIdSipHeaders.TestIsCallID;
begin
  Check(    TIdSipHeaders.IsCallID('Call-ID'),         'Call-ID');
  Check(    TIdSipHeaders.IsCallID('i'),               'i');
  Check(    TIdSipHeaders.IsCallID(CallIDHeaderFull),  'CallIDHeaderFull constant');
  Check(    TIdSipHeaders.IsCallID(CallIDHeaderShort), 'CallIDHeaderShort constant');
  Check(not TIdSipHeaders.IsCallID(''),                '''''');
  Check(not TIdSipHeaders.IsCallID(#0),                '#0');
  Check(not TIdSipHeaders.IsCallID(#$FF),              '#$FF');
  Check(not TIdSipHeaders.IsCallID('Content-Length'),  'Content-Length');
end;

procedure TestTIdSipHeaders.TestIsCompoundHeader;
begin
  Check(not TIdSipHeaders.IsCompoundHeader(''),                         '''''');
  Check(not TIdSipHeaders.IsCompoundHeader('New-Header'),               'New-Header');
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptHeader),               AcceptHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptEncodingHeader),       AcceptEncodingHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AcceptLanguageHeader),       AcceptLanguageHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AlertInfoHeader),            AlertInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AllowHeader),                AllowHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AuthenticationInfoHeader),   AuthenticationInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(AuthorizationHeader),        AuthorizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(CallIDHeaderFull),           CallIDHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(CallIDHeaderShort),          CallIDHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(CallInfoHeader),             CallInfoHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ContactHeaderFull),          ContactHeaderFull);
  Check(    TIdSipHeaders.IsCompoundHeader(ContactHeaderShort),         ContactHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentDispositionHeader),   ContentDispositionHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentEncodingHeaderFull),  ContentEncodingHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentEncodingHeaderShort), ContentEncodingHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLanguageHeader),      ContentLanguageHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLengthHeaderFull),    ContentLengthHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentLengthHeaderShort),   ContentLengthHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentTypeHeaderFull),      ContentTypeHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ContentTypeHeaderShort),     ContentTypeHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(CSeqHeader),                 CSeqHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(DateHeader),                 DateHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ErrorInfoHeader),            ErrorInfoHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ExpiresHeader),              ExpiresHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(FromHeaderFull),             FromHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(FromHeaderShort),            FromHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(InReplyToHeader),            InReplyToHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MaxForwardsHeader),          MaxForwardsHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MIMEVersionHeader),          MIMEVersionHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(MinExpiresHeader),           MinExpiresHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(OrganizationHeader),         OrganizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(PriorityHeader),             PriorityHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyAuthenticateHeader),    ProxyAuthenticateHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyAuthorizationHeader),   ProxyAuthorizationHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ProxyRequireHeader),         ProxyRequireHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(RecordRouteHeader),          RecordRouteHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ReplyToHeader),              ReplyToHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(RequireHeader),              RequireHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(RetryAfterHeader),           RetryAfterHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(RouteHeader),                RouteHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ServerHeader),               ServerHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(SubjectHeaderFull),          SubjectHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(SubjectHeaderShort),         SubjectHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(SupportedHeaderFull),        SupportedHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(SupportedHeaderShort),       SupportedHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(TimestampHeader),            TimestampHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(ToHeaderFull),               ToHeaderFull);
  Check(not TIdSipHeaders.IsCompoundHeader(ToHeaderShort),              ToHeaderShort);
  Check(not TIdSipHeaders.IsCompoundHeader(UnsupportedHeader),          UnsupportedHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(UserAgentHeader),            UserAgentHeader);
  Check(    TIdSipHeaders.IsCompoundHeader(ViaHeaderFull),              ViaHeaderFull);
  Check(    TIdSipHeaders.IsCompoundHeader(ViaHeaderShort),             ViaHeaderShort);
  Check(    TIdSipHeaders.IsCompoundHeader(WarningHeader),              WarningHeader);
  Check(not TIdSipHeaders.IsCompoundHeader(WWWAuthenticateHeader),      WWWAuthenticateHeader);
end;

procedure TestTIdSipHeaders.TestIsContact;
begin
  Check(    TIdSipHeaders.IsContact('Contact'),          'Contact');
  Check(    TIdSipHeaders.IsContact(ContactHeaderFull),  'ContactFull constant');
  Check(    TIdSipHeaders.IsContact('m'),                'm');
  Check(    TIdSipHeaders.IsContact(ContactHeaderShort), 'ContactShort constant');
  Check(not TIdSipHeaders.IsContact(''),                 '''''');
  Check(not TIdSipHeaders.IsContact('Via'),              'Via');
  Check(    TIdSipHeaders.IsContact('CoNTaCt'),          'CoNTaCt');
end;

procedure TestTIdSipHeaders.TestIsContentLength;
begin
  Check(    TIdSipHeaders.IsContentLength('Content-Length'),         'Content-Length');
  Check(    TIdSipHeaders.IsContentLength(ContentLengthHeaderFull),  'ContentLengthHeaderFull constant');
  Check(    TIdSipHeaders.IsContentLength('l'),                      'l');
  Check(    TIdSipHeaders.IsContentLength(ContentLengthHeaderShort), 'ContentLengthHeaderShort constant');
  Check(not TIdSipHeaders.IsContentLength(''),                       '''''');
  Check(not TIdSipHeaders.IsContentLength('Via'),                    'Via');
  Check(    TIdSipHeaders.IsContentLength('content-LeNgTh'),         'content-LeNgTh');
end;

procedure TestTIdSipHeaders.TestIsCSeq;
begin
  Check(not TIdSipHeaders.IsCSeq(''),         '''''');
  Check(    TIdSipHeaders.IsCSeq('cSeQ'),     'cSeQ');
  Check(    TIdSipHeaders.IsCSeq(CSeqHeader), 'CSeqHeader constant');
end;

procedure TestTIdSipHeaders.TestIsEmpty;
begin
  CheckEquals(0, Self.Headers.Count, 'Sanity check on test entry');
  Check(Self.Headers.IsEmpty, 'IsEmpty with 0 headers');
  Self.Headers.Add(RouteHeader).Value := '<sip:127.0.0.2>';
  Check(not Self.Headers.IsEmpty, 'IsEmpty after Add');
end;

procedure TestTIdSipHeaders.TestIsErrorInfo;
begin
  Check(not TIdSipHeaders.IsErrorInfo(''),              '''''');
  Check(    TIdSipHeaders.IsErrorInfo('Error-INFO'),    'Error-INFO');
  Check(    TIdSipHeaders.IsErrorInfo(ErrorInfoHeader), 'ErrorInfoHeader constant');
end;

procedure TestTIdSipHeaders.TestIsFrom;
begin
  Check(    TIdSipHeaders.IsFrom('From'),          'From');
  Check(    TIdSipHeaders.IsFrom(FromHeaderFull),  'FromHeaderFull constant');
  Check(    TIdSipHeaders.IsFrom('f'),             'f');
  Check(    TIdSipHeaders.IsFrom(FromHeaderShort), 'FromShort constant');
  Check(not TIdSipHeaders.IsFrom(''),              '''''');
  Check(not TIdSipHeaders.IsFrom('Via'),           'Via');
  Check(    TIdSipHeaders.IsFrom('fRoM'),          'fRoM');
end;

procedure TestTIdSipHeaders.TestIsMaxForwards;
begin
  Check(not TIdSipHeaders.IsMaxForwards(''),                '''''');
  Check(    TIdSipHeaders.IsMaxForwards('max-FORWARDS'),    'max-FORWARDS');
  Check(    TIdSipHeaders.IsMaxForwards(MaxForwardsHeader), 'MaxForwardsHeader constant');
end;

procedure TestTIdSipHeaders.TestIsRecordRoute;
begin
  Check(not TIdSipHeaders.IsRecordRoute(''),                '''''');
  Check(    TIdSipHeaders.IsRecordRoute('record-rOuTe'),    'record-rOuTe');
  Check(    TIdSipHeaders.IsRecordRoute(RecordRouteHeader), 'RecordRouteHeader constant');
end;

procedure TestTIdSipHeaders.TestIsRoute;
begin
  Check(not TIdSipHeaders.IsRoute(''),                '''''');
  Check(    TIdSipHeaders.IsRoute('rOuTe'),    'rOuTe');
  Check(    TIdSipHeaders.IsRoute(RouteHeader), 'RouteHeader constant');
end;

procedure TestTIdSipHeaders.TestIsTo;
begin
  Check(not TIdSipHeaders.IsTo(''),            '''''');
  Check(not TIdSipHeaders.IsTo('Tot'),         'Tot');
  Check(    TIdSipHeaders.IsTo('To'),          'To');
  Check(    TIdSipHeaders.IsTo('t'),           't');
  Check(    TIdSipHeaders.IsTo(ToHeaderFull),  'ToHeaderFull constant');
  Check(    TIdSipHeaders.IsTo(ToHeaderShort), 'ToHeaderShort constant');
end;

procedure TestTIdSipHeaders.TestIsVia;
begin
  Check(    TIdSipHeaders.IsVia('Via'),                  'Via');
  Check(    TIdSipHeaders.IsVia(ViaHeaderFull),          'ViaFull constant');
  Check(    TIdSipHeaders.IsVia('v'),                    'v');
  Check(    TIdSipHeaders.IsVia(ViaHeaderShort),         'ViaShort constant');
  Check(not TIdSipHeaders.IsVia(''),                     '''''');
  Check(not TIdSipHeaders.IsVia('Content-Length'),       'Content-Length');
  Check(    TIdSipHeaders.IsVia('Via:'),                 'Via:');
  Check(    TIdSipHeaders.IsVia('ViA'),                  'ViA');
end;

procedure TestTIdSipHeaders.TestIsWarning;
begin
  Check(not TIdSipHeaders.IsWarning(''),            '''''');
  Check(    TIdSipHeaders.IsWarning('warnING'),     'warnING');
  Check(    TIdSipHeaders.IsWarning(WarningHeader), 'WarningHeader constant');
end;

procedure TestTIdSipHeaders.TestIteratorVisitsAllHeaders;
var
  X, Y, Z: TIdSipHeader;
begin
  X := Self.Headers.Add('X-X-X');
  Y := Self.Headers.Add('X-X-Y');
  Z := Self.Headers.Add('X-X-Z');

  Self.Headers.First;
  while Self.Headers.HasNext do begin
    Self.Headers.CurrentHeader.Params['foo'] := 'bar';
    Self.Headers.Next;
  end;

  CheckEquals('bar', X.Params['foo'], 'X wasn''t visited by iterator');
  CheckEquals('bar', Y.Params['foo'], 'Y wasn''t visited by iterator');
  CheckEquals('bar', Z.Params['foo'], 'Z wasn''t visited by iterator');
end;

procedure TestTIdSipHeaders.TestRemove;
var
  X, Y, Z: TIdSipHeader;
begin
  X := Self.Headers.Add('X-X-X');
  Y := Self.Headers.Add('X-X-Y');
  Z := Self.Headers.Add('X-X-Z');

  Self.Headers.Remove(X);
  CheckEquals(Self.Headers.Items[0].AsString, Y.AsString, 'Wrong header removed (0)');
  CheckEquals(Self.Headers.Items[1].AsString, Z.AsString, 'Wrong header removed (1)');
end;

procedure TestTIdSipHeaders.TestRemoveAll;
begin
  Self.Headers.Add('Foo');
  Self.Headers.Add('Bar');
  Self.Headers.Add('Foo');

  Self.Headers.RemoveAll('foo');
  CheckEquals(1, Self.Headers.Count, 'Header count');
  CheckEquals('Bar', Self.Headers.Items[0].Name, 'Wrong headers removed');
end;

//******************************************************************************
//* TestTIdSipAuthorizations                                                   *
//******************************************************************************
//* TestTIdSipAuthorizations Public methods ************************************

procedure TestTIdSipAuthorizations.SetUp;
begin
  inherited SetUp;

  Self.Headers  := TIdSipHeaders.Create;
  Self.Authorizations := TIdSipAuthorizations.Create(Self.Headers);
end;

procedure TestTIdSipAuthorizations.TearDown;
begin
  Self.Authorizations.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipAuthorizations Published methods *********************************

procedure TestTIdSipAuthorizations.TestCreateOnEmptySet;
var
  Cnts:       TIdSipAuthorizations;
  NewHeader: TIdSipHeader;
begin
  Cnts := TIdSipAuthorizations.Create;
  try
    CheckEquals(0, Cnts.Count, 'Initial list');

    NewHeader := TIdSipAuthorizationHeader.Create;
    try
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a new Authorization');
    finally
      NewHeader.Free;
    end;

    NewHeader := TIdSipCallIdHeader.Create;
    try
      NewHeader.Value := '1'; // otherwise you have an invalid Call-ID
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a non-Authorization');
    finally
      NewHeader.Free;
    end;
  finally
    Cnts.Free;
  end;
end;

procedure TestTIdSipAuthorizations.TestCurrentAuthorization;
var
  NewAuthorization: TIdSipHeader;
begin
  Check(nil = Self.Authorizations.CurrentAuthorization,
        'No headers');

  Self.Headers.Add(ViaHeaderFull);
  Check(nil = Self.Authorizations.CurrentAuthorization,
        'No Authorizations');

  NewAuthorization := Self.Headers.Add(AuthorizationHeader);
  Self.Authorizations.First;
  Check(NewAuthorization = Self.Authorizations.CurrentAuthorization,
        'First Authorization');

  NewAuthorization := Self.Headers.Add(AuthorizationHeader);
  Self.Authorizations.First;
  Self.Authorizations.Next;
  Check(NewAuthorization = Self.Authorizations.CurrentAuthorization,
        'Second Authorization');
end;

//******************************************************************************
//* TestTIdSipContacts                                                         *
//******************************************************************************
//* TestTIdSipContacts Public methods ******************************************

procedure TestTIdSipContacts.SetUp;
begin
  inherited SetUp;

  Self.Headers  := TIdSipHeaders.Create;
  Self.Contacts := TIdSipContacts.Create(Self.Headers);
end;

procedure TestTIdSipContacts.TearDown;
begin
  Self.Contacts.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipContacts Published methods ***************************************

procedure TestTIdSipContacts.TestContactFor;
var
  From: TIdSipFromHeader;
  H:    TIdSipHeader;
begin
  From := TIdSipFromHeader.Create;
  try
    From.Address.Uri := 'sip:case@fried-neurons.org';

    Check(nil = Self.Contacts.ContactFor(From), 'Empty set');

    Self.Contacts.Add(ContactHeaderFull).Value := From.Value + '1';
    H := Self.Contacts.Add(ContactHeaderFull);
    H.Value := From.Value;
    Self.Contacts.Add(ContactHeaderFull).Value := From.Value + '2';

    Check(H = Self.Contacts.ContactFor(From), 'Wrong header');
  finally
    From.Free;
  end;
end;

procedure TestTIdSipContacts.TestCreateOnEmptySet;
var
  Cnts:       TIdSipContacts;
  NewHeader: TIdSipHeader;
begin
  Cnts := TIdSipContacts.Create;
  try
    CheckEquals(0, Cnts.Count, 'Initial list');

    NewHeader := TIdSipContactHeader.Create;
    try
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a new contact');
    finally
      NewHeader.Free;
    end;

    NewHeader := TIdSipCallIdHeader.Create;
    try
      NewHeader.Value := '1'; // otherwise you have an invalid Call-ID
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a non-contact');
    finally
      NewHeader.Free;
    end;
  finally
    Cnts.Free;
  end;
end;

procedure TestTIdSipContacts.TestCurrentContact;
var
  NewContact: TIdSipHeader;
begin
  Check(nil = Self.Contacts.CurrentContact,
        'No headers');

  Self.Headers.Add(ViaHeaderFull);
  Check(nil = Self.Contacts.CurrentContact,
        'No Contacts');

  NewContact := Self.Headers.Add(ContactHeaderFull);
  Self.Contacts.First;
  Check(NewContact = Self.Contacts.CurrentContact,
        'First Contact');

  NewContact := Self.Headers.Add(ContactHeaderFull);
  Self.Contacts.First;
  Self.Contacts.Next;
  Check(NewContact = Self.Contacts.CurrentContact,
        'Second Contact');
end;

procedure TestTIdSipContacts.TestGruuFor;
const
  ArbAddress = 'sip:wintermute@tessier-ashpool.co.luna';
  Gruu       = 'sip:wintermute@tessier-ashpool.co.luna;opaque=13';
  InstanceID = '<urn:uuid:00000000-0000-0000-0000-000000000000>';
var
  Contact: TIdSipContactHeader;
begin
  Contact := TIdSipContactHeader.Create;
  try
    Contact.Address.Uri := ArbAddress;
    Contact.SipInstance := InstanceID;
    CheckEquals('', Self.Contacts.GruuFor(Contact), 'Empty set');

    // A header for some UA other than Self.Core that happens to have the same
    // +sip.instance value
    Self.Contacts.Add(ContactHeaderFull).Value := ArbAddress + '.luna'
                    + ';' + SipInstanceParam + '="' + InstanceID + '"'
                    + ';' + GruuParam + '="' + Gruu + '1"';
    CheckEquals('',
                Self.Contacts.GruuFor(Contact),
                'No Contact with that ' + SipInstanceParam  + ' found');

    // Another UA for the same AOR as Self.Core, but with a different
    // +sip.instance value
    Self.Contacts.Add(ContactHeaderFull).Value := ArbAddress
                    + ';' + SipInstanceParam + '="' + InstanceID + '1"'
                    + ';' + GruuParam + '="' + Gruu + '"';

    // Self.Core's binding
    Self.Contacts.Add(ContactHeaderFull).Value := ArbAddress
                    + ';' + SipInstanceParam + '="' + InstanceID + '"'
                    + ';' + GruuParam + '="' + Gruu + '"';

    CheckEquals(Gruu, Self.Contacts.GruuFor(Contact), 'Wrong contact');
  finally
    Contact.Free;
  end;
end;

procedure TestTIdSipContacts.TestHasContact;
var
  NewContact: TIdSipContactHeader;
begin
  NewContact := TIdSipContactHeader.Create;
  try
    NewContact.Value := '"Foo Bar" <sip:foo>';
    Check(not Self.Contacts.HasContact(NewContact),
          'Empty list');

    Self.Contacts.Add(ContactHeaderFull).Value := 'sip:foo';

    Check(Self.Contacts.HasContact(NewContact),
          'Contains header with same address');
  finally
    NewContact.Free;
  end;
end;

procedure TestTIdSipContacts.TestRemoveContact;
const
  UriOne = 'sip:bob@cubicle10a.biloxi.com';
  UriTwo = 'sip:bob@freephone.example.com';
var
  C:             TIdSipAddressHeader;
  OriginalCount: Integer;
begin
  Self.Contacts.Add(ContactHeaderFull).Value := UriOne;
  Self.Contacts.Add(ContactHeaderFull).Value := UriTwo;

  OriginalCount := Self.Contacts.Count;

  C := TIdSipContactHeader.Create;
  try
    C.Value := UriTwo;

    Self.Contacts.RemoveContact(C);

    Self.Contacts.First;
    CheckEquals(UriOne, Self.Contacts.CurrentContact.Address.AsString,
                'Wrong Contact removed');

    Check(Self.Contacts.Count < OriginalCount, 'No Contact removed');
  finally
    C.Free;
  end;
end;

procedure TestTIdSipContacts.TestRemoveContactWithDuplicates;
var
  C: TIdSipContactHeader;
begin
  C := TIdSipContactHeader.Create;
  try
    C.Value := 'sip:case@unit55.jacks.example.com';
    Self.Contacts.Add(C);
    Self.Contacts.Add(C);

    Self.Contacts.RemoveContact(C);
    Check(Self.Contacts.IsEmpty, 'All matching Contacts not removed');
  finally
    C.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipExpiresHeaders                                                   *
//******************************************************************************
//* TestTIdSipExpiresHeaders Public methods ************************************

procedure TestTIdSipExpiresHeaders.SetUp;
begin
  inherited SetUp;

  Self.Headers        := TIdSipHeaders.Create;
  Self.ExpiresHeaders := TIdSipExpiresHeaders.Create(Self.Headers);
end;

procedure TestTIdSipExpiresHeaders.TearDown;
begin
  Self.ExpiresHeaders.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipExpiresHeaders Published methods *********************************

procedure TestTIdSipExpiresHeaders.TestCurrentExpires;
begin
  CheckEquals(0,
              Self.ExpiresHeaders.CurrentExpires,
              'No headers');

  Self.Headers.Add(ViaHeaderFull);
  CheckEquals(0,
              Self.ExpiresHeaders.CurrentExpires,
              'No ExpiresHeaders');

  Self.Headers.Add(ExpiresHeader).Value := '99';
  Self.ExpiresHeaders.First;
  CheckEquals(99,
              Self.ExpiresHeaders.CurrentExpires,
              'First Expires');

  Self.Headers.Add(ExpiresHeader).Value := '22';
  Self.ExpiresHeaders.First;
  Self.ExpiresHeaders.Next;
  CheckEquals(22,
              Self.ExpiresHeaders.CurrentExpires,
              'First Expires');
end;

//******************************************************************************
//* TestTIdSipProxyAuthorizations                                              *
//******************************************************************************
//* TestTIdSipProxyAuthorizations Public methods *******************************

procedure TestTIdSipProxyAuthorizations.SetUp;
begin
  inherited SetUp;

  Self.Headers  := TIdSipHeaders.Create;
  Self.ProxyAuthorizations := TIdSipProxyAuthorizations.Create(Self.Headers);
end;

procedure TestTIdSipProxyAuthorizations.TearDown;
begin
  Self.ProxyAuthorizations.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipProxyAuthorizations Published methods ****************************

procedure TestTIdSipProxyAuthorizations.TestCreateOnEmptySet;
var
  Cnts:       TIdSipProxyAuthorizations;
  NewHeader: TIdSipHeader;
begin
  Cnts := TIdSipProxyAuthorizations.Create;
  try
    CheckEquals(0, Cnts.Count, 'Initial list');

    NewHeader := TIdSipProxyAuthorizationHeader.Create;
    try
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a new ProxyAuthorization');
    finally
      NewHeader.Free;
    end;

    NewHeader := TIdSipCallIdHeader.Create;
    try
      NewHeader.Value := '1'; // otherwise you have an invalid Call-ID
      Cnts.Add(NewHeader);
      CheckEquals(1, Cnts.Count, 'Added a non-ProxyAuthorization');
    finally
      NewHeader.Free;
    end;
  finally
    Cnts.Free;
  end;
end;

procedure TestTIdSipProxyAuthorizations.TestCurrentProxyAuthorization;
var
  NewProxyAuthorization: TIdSipHeader;
begin
  Check(nil = Self.ProxyAuthorizations.CurrentProxyAuthorization,
        'No headers');

  Self.Headers.Add(ViaHeaderFull);
  Check(nil = Self.ProxyAuthorizations.CurrentProxyAuthorization,
        'No ProxyAuthorizations');

  NewProxyAuthorization := Self.Headers.Add(ProxyAuthorizationHeader);
  Self.ProxyAuthorizations.First;
  Check(NewProxyAuthorization = Self.ProxyAuthorizations.CurrentProxyAuthorization,
        'First ProxyAuthorization');

  NewProxyAuthorization := Self.Headers.Add(ProxyAuthorizationHeader);
  Self.ProxyAuthorizations.First;
  Self.ProxyAuthorizations.Next;
  Check(NewProxyAuthorization = Self.ProxyAuthorizations.CurrentProxyAuthorization,
        'Second ProxyAuthorization');
end;

//******************************************************************************
//* TestTIdSipRoutePath                                                        *
//******************************************************************************
//* TestTIdSipRoutePath Public methods *****************************************

procedure TestTIdSipRoutePath.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;
  Self.Routes  := TIdSipRoutePath.Create(Self.Headers);
end;

procedure TestTIdSipRoutePath.TearDown;
begin
  Self.Routes.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipRoutePath Published methods **************************************

procedure TestTIdSipRoutePath.TestAddRoute;
const
  ProxyUri = 'sip:proxy.tessier-ashpool.co.luna';
var
  NewRoute: TIdSipUri;
begin
  Check(Self.Routes.IsEmpty,
        'Precondition: Self.Routes must be empty');

  NewRoute := TIdSipUri.Create(ProxyUri);
  try
    Self.Routes.AddRoute(NewRoute);
    Self.Routes.First;
    Check(Self.Routes.HasNext, 'Route not added');
    CheckEquals(NewRoute.Uri,
                Self.Routes.CurrentRoute.Address.Uri,
                'Wrong URI added');
  finally
    NewRoute.Free;
  end;
end;

procedure TestTIdSipRoutePath.TestCreateOnEmptySet;
var
  Rts:       TIdSipRoutePath;
  NewHeader: TIdSipHeader;
begin
  Rts := TIdSipRoutePath.Create;
  try
    CheckEquals(0, Rts.Count, 'Initial list');

    NewHeader := TIdSipRouteHeader.Create;
    try
      NewHeader.Value := '<sip:proxy.tessier-ashpool.co.luna>';
      Rts.Add(NewHeader);
      CheckEquals(1, Rts.Count, 'Added a new Route');
    finally
      NewHeader.Free;
    end;

    NewHeader := TIdSipCallIdHeader.Create;
    try
      NewHeader.Value := 'foo@bar';
      Rts.Add(NewHeader);
      CheckEquals(1, Rts.Count, 'Added a non-Route');
    finally
      NewHeader.Free;
    end;
  finally
    Rts.Free;
  end;
end;

procedure TestTIdSipRoutePath.TestCurrentRoute;
var
  NewRoute: TIdSipHeader;
begin
  Check(nil = Self.Routes.CurrentRoute,
        'No headers');

  Self.Headers.Add(ViaHeaderFull);
  Check(nil = Self.Routes.CurrentRoute,
        'No Routes');

  NewRoute := Self.Headers.Add(RouteHeader);
  Self.Routes.First;
  Check(NewRoute = Self.Routes.CurrentRoute,
        'First Route');

  NewRoute := Self.Headers.Add(RouteHeader);
  Self.Routes.First;
  Self.Routes.Next;
  Check(NewRoute = Self.Routes.CurrentRoute,
        'Second Route');
end;

procedure TestTIdSipRoutePath.TestGetAllButFirst;
var
  Expected: TIdSipRoutePath;
  Received: TIdSipRoutePath;
begin
  Expected := TIdSipRoutePath.Create;
  try
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.2>';
    Expected.Add(RouteHeader).Value := '<sip:127.0.0.3>';

    Self.Routes.Add(RouteHeader).Value := '<sip:127.0.0.1>';
    Self.Routes.Add(Expected);

    Received := Self.Routes.GetAllButFirst;
    try
      Check(Expected.Equals(Received), 'Unexpected header set returned');
    finally
      Received.Free;
    end;
  finally
    Expected.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipViaPath                                                          *
//******************************************************************************
//* TestTIdSipViaPath Public methods *******************************************

procedure TestTIdSipViaPath.SetUp;
begin
  inherited SetUp;

  Self.Headers := TIdSipHeaders.Create;
  Self.Path := TIdSipViaPath.Create(Self.Headers);
end;

procedure TestTIdSipViaPath.TearDown;
begin
  Self.Path.Free;
  Self.Headers.Free;

  inherited TearDown;
end;

//* TestTIdSipViaPath Published methods ****************************************

procedure TestTIdSipViaPath.TestAddAndLastHop;
var
  Hop: TIdSipViaHeader;
begin
  CheckEquals(0, Self.Path.Length, 'Has hops, but is newly created');

  Hop := Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;

  Hop.SentBy     := '127.0.0.1';
  Hop.Port       := 5060;
  Hop.SipVersion := 'SIP/2.0';
  Hop.Transport  := SctpTransport;

  CheckEquals(1, Self.Path.Length, 'Has no hops after Add');

  CheckEquals(ViaHeaderFull, Self.Path.LastHop.Name,       'Name');
  CheckEquals('127.0.0.1',   Self.Path.LastHop.SentBy,     'SentBy');
  CheckEquals(5060,          Self.Path.LastHop.Port,       'Port');
  CheckEquals('SIP/2.0',     Self.Path.LastHop.SipVersion, 'SipVersion');
  CheckEquals(SctpTransport, Self.Path.LastHop.Transport,  'Transport');
end;

procedure TestTIdSipViaPath.TestClear;
begin
  CheckEquals(0, Self.Headers.Count, 'Unexpected headers');
  Self.Headers.Add(ViaHeaderFull);
  Self.Headers.Add(ViaHeaderFull);
  Self.Headers.Add(ViaHeaderFull);
  CheckEquals(3, Self.Headers.Count, 'After 3 Add()s');

  Self.Path.Clear;
  CheckEquals(0, Self.Headers.Count, 'After Clear()');
end;

procedure TestTIdSipViaPath.TestCurrentHop;
var
  NewVia: TIdSipHeader;
begin
  Check(nil = Self.Path.CurrentHop,
        'No headers');

  Self.Headers.Add(RouteHeader);
  Check(nil = Self.Path.CurrentHop,
        'No Path');

  NewVia := Self.Headers.Add(ViaHeaderFull);
  Self.Path.First;
  Check(NewVia = Self.Path.CurrentHop,
        'First Via');

  NewVia := Self.Headers.Add(ViaHeaderFull);
  Self.Path.First;
  Self.Path.Next;
  Check(NewVia = Self.Path.CurrentHop,
        'Second Via');
end;

procedure TestTIdSipViaPath.TestRemoveLastHop;
begin
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '1';
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '2';
  (Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader).SentBy := '3';

  CheckEquals('1', Self.Path.LastHop.SentBy, 'Sanity check');
  Self.Path.RemoveLastHop;
  CheckEquals('2', (Self.Headers.Items[0] as TIdSipViaHeader).SentBy, 'new LastHop');
  CheckEquals('3', (Self.Headers.Items[1] as TIdSipViaHeader).SentBy, 'First Hop');
end;

initialization
  RegisterTest('SIP Message Headers', Suite);
end.

