{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestConfigUtils;

interface

uses
  ConfigUtils, TestFramework;

type
  TestFunctions = class(TTestCase)
  public
    procedure CheckDefaultFalseStrings;
    procedure CheckDefaultTrueStrings;
  published
    procedure TestCustomStrAsBool;
    procedure TestDefaultStrAsBool;
    procedure TestSetDefaultTruthStrings;
    procedure TestStrAsBoolDef;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('ConfigUtils unit tests');
  Result.AddTest(TestFunctions.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.CheckDefaultFalseStrings;
begin
  Check(not StrAsBool('false'),   'false');
  Check(not StrAsBool('FALSE'),   'FALSE');
  Check(not StrAsBool('no'),      'no');
  Check(not StrAsBool('disable'), 'disable');
  Check(not StrAsBool('off'),     'off');
  Check(not StrAsBool('0'),       '0');
end;

procedure TestFunctions.CheckDefaultTrueStrings;
begin
  Check(StrAsBool('true'),   'true');
  Check(StrAsBool('TRUE'),   'TRUE');
  Check(StrAsBool('yes'),    'yes');
  Check(StrAsBool('enable'), 'enable');
  Check(StrAsBool('on'),     'on');
  Check(StrAsBool('1'),      '1');
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestCustomStrAsBool;
begin
  SetTrueValues(['bar']);
  SetFalseValues(['foo']);
  try
    Check(StrAsBool('bar'), 'bar');
    Check(not StrAsBool('foo'), 'foo');
    Check(not StrAsBool('true'), '"true" is no longer true');
  finally
    // Restore the global values to sanity.
    SetDefaultTruthStrings;
  end;
end;

procedure TestFunctions.TestDefaultStrAsBool;
begin
  CheckDefaultFalseStrings;
  CheckDefaultTrueStrings;

  // Unknown values default to "false".
  Check(not StrAsBool(''),    'The empty string');
  Check(not StrAsBool('1/2'), '1/2');
end;

procedure TestFunctions.TestSetDefaultTruthStrings;
begin
  SetTrueValues(['bar']);
  SetFalseValues(['foo']);

  SetDefaultTruthStrings;

  CheckDefaultFalseStrings;
  CheckDefaultTrueStrings;
end;

procedure TestFunctions.TestStrAsBoolDef;
begin
  Check(    StrAsBoolDef('unknown', true),  '"unknown", defaulting to true');
  Check(not StrAsBoolDef('unknown', false), '"unknown", defaulting to false');
end;

initialization
  RegisterTest('Configuration utility tests', Suite);
end.
