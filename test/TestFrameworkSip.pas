unit TestFrameworkSip;

interface

uses
  IdURI, TestFramework;

type
  TTestCaseSip = class(TTestCase)
    procedure CheckEquals(Expected, Received: TIdURI; Message: String); overload;
  end;

implementation

//******************************************************************************
//* TTestCaseSip                                                               *
//******************************************************************************
//* TTestCaseSip Public methods ************************************************

procedure TTestCaseSip.CheckEquals(Expected, Received: TIdURI; Message: String);
begin
  CheckEquals(Expected.GetFullURI, Received.GetFullURI, Message);
end;

end.
 