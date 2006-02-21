{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipSctpTransport;

interface

uses
  IdSipTransport, TestFrameworkSipTransport;

type
  TestTIdSipSCTPTransport = class(TestTIdSipTransport)
  protected
    function  TransportType: TIdSipTransportClass; override;
  published
    procedure TestGetTransportType;
    procedure TestIsReliable;
    procedure TestIsSecure;
  end;

implementation

uses
  IdSipMessage, IdSipSCTPTransport, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipSctpTransport unit tests');
// Commented out because the SCTP transport's nothing but a stub
//  Result.AddTest(TestTIdSipSCTPTransport.Suite);
end;

//******************************************************************************
//* TestTIdSipSCTPTransport                                                    *
//******************************************************************************
//* TestTIdSipSCTPTransport Protected methods **********************************

function TestTIdSipSCTPTransport.TransportType: TIdSipTransportClass;
begin
  Result := TIdSipSCTPTransport;
end;

//* TestTIdSipSCTPTransport Published methods **********************************

procedure TestTIdSipSCTPTransport.TestGetTransportType;
begin
  CheckEquals(SctpTransport,
              Self.HighPortTransport.GetTransportType,
              'Transport type');
end;

procedure TestTIdSipSCTPTransport.TestIsReliable;
begin
  Check(Self.HighPortTransport.IsReliable, 'SCTP transport not marked as reliable');
end;

procedure TestTIdSipSCTPTransport.TestIsSecure;
begin
  Check(not Self.HighPortTransport.IsSecure, 'SCTP transport marked as secure');
end;

initialization
  RegisterTest('SCTP transport', Suite);
end.
