{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipServerNotifier;

interface

uses
  IdSipMessage, IdSipServerNotifier, TestFramework, TestFrameworkSip;

type
  TestTIdSipServerNotifier = class(TTestCase)
  private
    Binding:  TIdSipConnectionBindings;
    Listener: TIdSipTestMessageListener;
    Notifier: TIdSipServerNotifier;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessageListener;
    procedure TestNotifyListenersOfException;
    procedure TestNotifyListenersOfRequest;
    procedure TestNotifyListenersOfResponse;
    procedure TestNotifyListenersOfMalformedMessage;
    procedure TestRemoveMessageListener;
  end;

implementation

uses
  SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipServerNotifier unit tests');
  Result.AddTest(TestTIdSipServerNotifier.Suite);
end;

//******************************************************************************
//* TestTIdSipServerNotifier                                                   *
//******************************************************************************
//* TestTIdSipServerNotifier Public methods ************************************

procedure TestTIdSipServerNotifier.SetUp;
begin
  inherited SetUp;

  Self.Notifier := TIdSipServerNotifier.Create;
  Self.Listener := TIdSipTestMessageListener.Create;

  Self.Notifier.AddMessageListener(Self.Listener);

  Self.Binding.LocalIP   := '2002:5156:4019:1::1';
  Self.Binding.LocalPort := 5060;
  Self.Binding.PeerIP    := '192.168.0.1';
  Self.Binding.PeerPort  := 15060;
end;

procedure TestTIdSipServerNotifier.TearDown;
begin
  Self.Notifier.RemoveMessageListener(Self.Listener);

  Self.Listener.Free;
  Self.Notifier.Free;

  inherited TearDown;
end;

//* TestTIdSipServerNotifier Published methods *********************************

procedure TestTIdSipServerNotifier.TestAddMessageListener;
begin
  Self.Notifier.NotifyListenersOfMalformedMessage('foo', 'bar');

   Check(Self.Listener.MalformedMessage,
        'Listener not notified');
end;

procedure TestTIdSipServerNotifier.TestNotifyListenersOfException;
var
  E: Exception;
begin
  E := Exception.Create('');
  try
    Self.Notifier.NotifyListenersOfException(E, 'Reason');

    Check(Self.Listener.Exception,
          'Listener not notified');
    Check(E = Self.Listener.ExceptionParam,
          'Exception param');
    CheckEquals('Reason',
                Self.Listener.ReasonParam,
               'Reason param');
  finally
    E.Free;
  end;
end;

procedure TestTIdSipServerNotifier.TestNotifyListenersOfRequest;
var
  Req: TIdSipRequest;
begin
  Req := TIdSipRequest.Create;
  try
    Self.Notifier.NotifyListenersOfRequest(Req, Self.Binding);

    Check(Self.Listener.ReceivedRequest,
          'Listener not notified');
    Check(Req = Self.Listener.RequestParam,
          'Request param');

    CheckEquals(Self.Binding.LocalIP,
                Self.Listener.ReceivedFromParam.LocalIP,
                'ReceivedFrom param (LocalIP');
    CheckEquals(Self.Binding.LocalPort,
                Self.Listener.ReceivedFromParam.LocalPort,
                'ReceivedFrom param (LocalPort');
    CheckEquals(Self.Binding.PeerIP,
                Self.Listener.ReceivedFromParam.PeerIP,
                'ReceivedFrom param (PeerIP');
    CheckEquals(Self.Binding.PeerPort,
                Self.Listener.ReceivedFromParam.PeerPort,
                'ReceivedFrom param (PeerPort');
  finally
    Req.Free;
  end;
end;

procedure TestTIdSipServerNotifier.TestNotifyListenersOfResponse;
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    Self.Notifier.NotifyListenersOfResponse(Res, Self.Binding);

    Check(Self.Listener.ReceivedResponse,
          'Listener not notified');
    Check(Res = Self.Listener.ResponseParam,
          'Response param');

    CheckEquals(Self.Binding.LocalIP,
                Self.Listener.ReceivedFromParam.LocalIP,
                'ReceivedFrom param (LocalIP');
    CheckEquals(Self.Binding.LocalPort,
                Self.Listener.ReceivedFromParam.LocalPort,
                'ReceivedFrom param (LocalPort');
    CheckEquals(Self.Binding.PeerIP,
                Self.Listener.ReceivedFromParam.PeerIP,
                'ReceivedFrom param (PeerIP');
    CheckEquals(Self.Binding.PeerPort,
                Self.Listener.ReceivedFromParam.PeerPort,
                'ReceivedFrom param (PeerPort');
  finally
    Res.Free;
  end;
end;

procedure TestTIdSipServerNotifier.TestNotifyListenersOfMalformedMessage;
begin
  Self.Notifier.NotifyListenersOfMalformedMessage('foo', 'bar');

  Check(Self.Listener.MalformedMessage,
        'Listener not notified');
  CheckEquals('foo',
              Self.Listener.MalformedMessageParam,
              'MalformedMessage param');
  CheckEquals('bar',
              Self.Listener.ReasonParam,
              'Reason param');
end;

procedure TestTIdSipServerNotifier.TestRemoveMessageListener;
begin
  Self.Notifier.RemoveMessageListener(Self.Listener);
  Self.Notifier.NotifyListenersOfMalformedMessage('foo', 'bar');

   Check(not Self.Listener.MalformedMessage,
        'Listener notified, therefore not removed');
end;

initialization
  RegisterTest('SIP server notifier', Suite);
end.
