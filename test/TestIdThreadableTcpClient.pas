{
  (c) 2009 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdThreadableTcpClient;

interface

uses
  IdTcpServer, IdThreadableTcpClient, TestFramework;

type
  TestTIdThreadableTcpClient = class(TTestCase)
  private
    C: TIdThreadableTcpClient;
    S: TIdTcpServer;

    procedure DoNothing(Thread: TIdPeerThread);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnectSetsCachedBindings;
    procedure TestInstantiationSetsID;
  end;


implementation

uses
  IdConnectionBindings, IdIndyUtils, IdRegisteredObject, IdSocketHandle;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdThreadableTcpClient unit tests');
  Result.AddTest(TestTIdThreadableTcpClient.Suite);
end;

//******************************************************************************
//* TestTIdThreadableTcpClient                                                 *
//******************************************************************************
//* TestTIdThreadableTcpClient Public methods **********************************

procedure TestTIdThreadableTcpClient.SetUp;
begin
  inherited SetUp;

  Self.C := TIdThreadableTcpClient.Create(nil);
  Self.S := CreateTcpServer(TIdTcpServer);
  Self.S.OnExecute := Self.DoNothing;
  OpenOnFirstFreePort(S, '127.0.0.1', 8000);

  Self.C.Host := Self.S.Bindings[0].IP;
  Self.C.Port := Self.S.Bindings[0].Port;
end;

procedure TestTIdThreadableTcpClient.TearDown;
begin
  Self.S.Free;
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdThreadableTcpClient Private methods *********************************

procedure TestTIdThreadableTcpClient.DoNothing(Thread: TIdPeerThread);
begin
end;

//* TestTIdThreadableTcpClient Published methods *******************************

procedure TestTIdThreadableTcpClient.TestConnectSetsCachedBindings;
const
  OneSecond = 1000;
var
  B: TIdConnectionBindings;
begin
  Self.C.Connect(OneSecond);

  CheckEquals(Self.C.Socket.Binding.IP,       Self.C.CachedBindings.LocalIP,   'LocalIP');
  CheckEquals(Self.C.Socket.Binding.Port,     Self.C.CachedBindings.LocalPort, 'LocalPort');
  CheckEquals(Self.C.Socket.Binding.PeerIP,   Self.C.CachedBindings.PeerIP,    'PeerIP');
  CheckEquals(Self.C.Socket.Binding.PeerPort, Self.C.CachedBindings.PeerPort,  'PeerPort');
  CheckEquals(Self.C.Protocol,                Self.C.CachedBindings.Transport, 'Transport');

  B := Self.C.CachedBindings.Copy;
  try
    Self.C.Disconnect;

  CheckEquals(B.LocalIP,   Self.C.CachedBindings.LocalIP,   'LocalIP didn''t survive disconnection');
  CheckEquals(B.LocalPort, Self.C.CachedBindings.LocalPort, 'LocalPort didn''t survive disconnection');
  CheckEquals(B.PeerIP,    Self.C.CachedBindings.PeerIP,    'PeerIP didn''t survive disconnection');
  CheckEquals(B.PeerPort,  Self.C.CachedBindings.PeerPort,  'PeerPort didn''t survive disconnection');
  CheckEquals(B.Transport, Self.C.CachedBindings.Transport, 'Transport didn''t survive disconnection');
  finally
    B.Free;
  end;
end;

procedure TestTIdThreadableTcpClient.TestInstantiationSetsID;
begin
  CheckNotEquals('', OidAsString(Self.C.ID), 'ID not set');
end;

initialization
  RegisterTest('Threadable TCP clients', Suite);
end.
