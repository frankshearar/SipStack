{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdObservable;

interface

uses
  IdInterfacedObject, IdObservable, TestFramework;

type
  TestTIdObservable = class(TTestCase)
  private
    Observable: TIdObservable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNotifyWithData;
    procedure TestOnChanged;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdObservable unit tests');
  Result.AddTest(TestTIdObservable.Suite);
end;

//******************************************************************************
//* TestTIdObservable                                                          *
//******************************************************************************
//* TestTIdObservable Public methods *******************************************

procedure TestTIdObservable.SetUp;
begin
  inherited SetUp;

  Self.Observable := TIdObservable.Create;
end;

procedure TestTIdObservable.TearDown;
begin
  Self.Observable.Free;

  inherited TearDown;
end;

//* TestTIdObservable Published methods ****************************************

procedure TestTIdObservable.TestNotifyWithData;
var
  L1: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    Self.Observable.AddObserver(L1);

    Self.Observable.NotifyListenersOfChange(Self);

    Check(L1.Changed,     'L1 not notified of change');
    Check(Self = L1.Data, 'Data object');
  finally
    L1.Free;
  end;
end;

procedure TestTIdObservable.TestOnChanged;
var
  L1, L2: TIdObserverListener;
begin
  L1 := TIdObserverListener.Create;
  try
    L2 := TIdObserverListener.Create;
    try
      Self.Observable.AddObserver(L1);
      Self.Observable.AddObserver(L2);

      Self.Observable.NotifyListenersOfChange;

      Check(L1.Changed, 'L1 not notified of change');
      Check(L2.Changed, 'L1 not notified of change');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

initialization
  RegisterTest('Observable object', Suite);
end.
