unit TestIdNotification;

interface

uses
  IdInterfacedObject, IdNotification, TestFramework;

type
  IIdFoo = interface
    ['{0B189835-280E-41B6-9D93-EEDED5BA7EA3}']
    procedure Bar;
  end;

  TIdFoo = class(TIdInterfacedObject,
                 IIdFoo)
  private
    fBarCalled: Boolean;

    procedure Bar;
  public
    constructor Create;

    property BarCalled: Boolean read fBarCalled;
  end;

  TIdCallBar = class(TIdSipMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TestTIdNotificationList = class(TTestCase)
  private
    List: TIdNotificationList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddRemoveCount;
    procedure TestNotify;
  end;

implementation

uses
  SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdNotification unit tests');
  Result.AddTest(TestTIdNotificationList.Suite);
end;

//******************************************************************************
//* TIdFoo                                                                     *
//******************************************************************************
//* TIdFoo Public methods ******************************************************

constructor TIdFoo.Create;
begin
  inherited Create;

  Self.fBarCalled := false;
end;

//* TIdFoo Private methods *****************************************************

procedure TIdFoo.Bar;
begin
  Self.fBarCalled := true;
end;

//******************************************************************************
//* TIdCallBar                                                                 *
//******************************************************************************
//* TIdCallBar Public methods **************************************************

procedure TIdCallBar.Run(const Subject: IInterface);
begin
  (Subject as IIdFoo).Bar;
end;

//******************************************************************************
//* TestTIdNotificationList                                                    *
//******************************************************************************
//* TestTIdNotificationList Public methods *************************************

procedure TestTIdNotificationList.SetUp;
begin
  inherited SetUp;

  Self.List := TIdNotificationList.Create;
end;

procedure TestTIdNotificationList.TearDown;
begin
  Self.List.Free;

  inherited TearDown;
end;

//* TestTIdNotificationList Published methods **********************************

procedure TestTIdNotificationList.TestAddRemoveCount;
var
  O1, O2: TIdInterfacedObject;
begin
  O1 := TIdInterfacedObject.Create;
  try
    O2:= TIdInterfacedObject.Create;
    try
      CheckEquals(0, Self.List.Count, 'Empty list');
      Self.List.AddListener(O1);
      CheckEquals(1, Self.List.Count, '[O1]');
      Self.List.AddListener(O2);
      CheckEquals(2, Self.List.Count, '[O1, O2]');

      Self.List.RemoveListener(O1);
      CheckEquals(1, Self.List.Count, '[O2]');
      Self.List.RemoveListener(O2);
      CheckEquals(0, Self.List.Count, 'Empty list again');
    finally
      O2.Free;
    end;
  finally
    O1.Free;
  end;
end;

procedure TestTIdNotificationList.TestNotify;
var
  F1, F2: TIdFoo;
  BarCaller: TIdCallBar;
begin
  F1 := TIdFoo.Create;
  try
    F2 := TIdFoo.Create;
    try
      Self.List.AddListener(F1);
      Self.List.AddListener(F2);

      BarCaller := TIdCallBar.Create;
      try
        Self.List.Notify(BarCaller);
      finally
        BarCaller.Free;
      end;

      Check(F1.BarCalled, 'Method not invoked on F1');
      Check(F2.BarCalled, 'Method not invoked on F2');
    finally
      F2.Free;
    end;
  finally
    F1.Free;
  end;
end;

initialization
  RegisterTest('SIP Notification mechanism tests', Suite);
end.
