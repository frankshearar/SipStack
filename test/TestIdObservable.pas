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

  TIdObserverListener = class(TIdInterfacedObject,
                              IIdSipObserver)
  private
    fChanged: Boolean;
    fData:    TObject;
  public
    constructor Create;

    procedure OnChanged(Observed: TObject);

    property Changed: Boolean read fChanged;
    property Data:    TObject read fData;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPDiagnostics unit tests');
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

//******************************************************************************
//* TIdObserverListener                                                        *
//******************************************************************************
//* TIdObserverListener Public methods *****************************************

constructor TIdObserverListener.Create;
begin
  inherited Create;

  Self.fChanged := false;
  Self.fData    := nil;
end;

procedure TIdObserverListener.OnChanged(Observed: TObject);
begin
  Self.fChanged := true;
  Self.fData    := Observed;
end;

initialization
  RegisterTest('Observable object', Suite);
end.
