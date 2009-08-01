{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdIndyUtils;

interface

uses
  IdIndyUtils, IdSocketHandle, IdTcpServer, TestFramework;

type
  TestFunctions = class(TTestCase)
  private
    Sockets: TIdSocketHandles;
    Server:  TIdTcpServer;

    procedure DoNothing(Thread: TIdPeerThread);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBindingsToStrEmptyList;
    procedure TestBindingsToStrMultipleSockets;
    procedure TestBindingsToStrOneSocket;
    procedure TestOpenOnFirstFreePortOpenPort;
    procedure TestOpenOnFirstFreePortUsedPort;
    procedure TestOpenOnFirstFreePortUsedPorts;
    procedure TestRaiseSocketError;
    procedure TestSockOptToBool;
    procedure TestSocketUsesKeepAlive;
  end;

implementation

uses
  IdException, IdStackConsts, IdTcpClient, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdIndyUtils unit tests');
  Result.AddTest(TestFunctions.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.Sockets := TIdSocketHandles.Create(nil);

  Self.Server := CreateTcpServer(TIdTcpServer);
  Self.Server.OnExecute := Self.DoNothing;
  OpenOnFirstFreePort(Self.Server, '127.0.0.1', 5060);
end;

procedure TestFunctions.TearDown;
begin
  Self.Server.Free;
  Self.Sockets.Free;

  inherited TearDown;
end;

//* TestFunctions Private methods **********************************************

procedure TestFunctions.DoNothing(Thread: TIdPeerThread);
begin
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestBindingsToStrEmptyList;
begin
  Self.Sockets.DefaultPort := 1000;
  CheckEquals('*:' + IntToStr(Self.Sockets.DefaultPort), BindingsToStr(Self.Sockets), 'Empty socket list');
end;

procedure TestFunctions.TestBindingsToStrMultipleSockets;
begin
  Self.Sockets.Add;
  Self.Sockets.Add;

  Self.Sockets[0].IP := '127.0.0.1';
  Self.Sockets[0].Port := 9;
  Self.Sockets[1].IP := '127.0.0.2';
  Self.Sockets[1].Port := 80;

  CheckEquals('127.0.0.1:9, 127.0.0.2:80', BindingsToStr(Self.Sockets), 'Two-entry socket list');
end;

procedure TestFunctions.TestBindingsToStrOneSocket;
begin
  Self.Sockets.Add;

  Self.Sockets[0].IP := '127.0.0.1';
  Self.Sockets[0].Port := 9;

  CheckEquals('127.0.0.1:9', BindingsToStr(Self.Sockets), 'One-entry socket list');
end;

procedure TestFunctions.TestOpenOnFirstFreePortOpenPort;
var
  Address:  String;
  OpenPort: Integer;
begin
  // It is possible that OpenPort could be taken by another socket between
  // shutting down Server and starting it up: good luck with solving this
  // problem!
  Address  := Self.Server.Bindings[0].IP;
  OpenPort := Self.Server.Bindings[0].Port;
  Self.Server.Active := false;

  OpenOnFirstFreePort(Self.Server, Address, OpenPort);
  CheckEquals(OpenPort, Self.Server.Bindings[0].Port, 'Open port not used');
end;

procedure TestFunctions.TestOpenOnFirstFreePortUsedPort;
var
  NewServer: TIdTcpServer;
begin
  NewServer := TIdTCPServer.Create(nil);
  try
    OpenOnFirstFreePort(NewServer,
                        Self.Server.Bindings[0].IP,
                        Self.Server.Bindings[0].Port);

    Check(Self.Server.Bindings[0].Port < NewServer.Bindings[0].Port, 'Open port not skipped');
  finally
    NewServer.Free;
  end;
end;

procedure TestFunctions.TestOpenOnFirstFreePortUsedPorts;
var
  NewServer: TIdTcpServer;
begin
  Self.Server.Active := false;
  Self.Server.Bindings.Add;
  Self.Server.Bindings.Add;
  Self.Server.Bindings[1].IP   := Self.Server.Bindings[0].IP;
  Self.Server.Bindings[1].Port := Self.Server.Bindings[0].Port + 1;
  Self.Server.Bindings[2].IP   := Self.Server.Bindings[0].IP;
  Self.Server.Bindings[2].Port := Self.Server.Bindings[0].Port + 2;

  Self.Server.Active := true;

  NewServer := CreateTcpServer(TIdTCPServer);
  try
    OpenOnFirstFreePort(NewServer,
                        Self.Server.Bindings[0].IP,
                        Self.Server.Bindings[0].Port);

    Check(Self.Server.Bindings[2].Port < NewServer.Bindings[0].Port, 'Open port not skipped');
  finally
    NewServer.Free;
  end;
end;

procedure TestFunctions.TestRaiseSocketError;
var
  OriginalException: Exception;
begin
  try
    OriginalException := Exception.Create('');
    try
      RaiseSocketError(OriginalException, Self.Sockets);
    finally
      OriginalException.Free;
    end;

    Fail('No exception raised');
  except
    on EIdSocketError do;
  end;
end;

procedure TestFunctions.TestSockOptToBool;
begin
  Check(    SockOptToBool(Id_SO_True),      'Id_SO_True');
  Check(    SockOptToBool(Id_SO_True + 1),  'Id_SO_True + 1');
  Check(    SockOptToBool(Id_SO_False + 1), 'Id_SO_False + 1');
  Check(not SockOptToBool(Id_SO_False),     'Id_SO_False');
end;

procedure TestFunctions.TestSocketUsesKeepAlive;
const
  OneSecond = 1000;
var
  C: TIdTcpClient;
begin
  C := TIdTcpClient.Create(nil);
  try
    C.Host := Self.Server.Bindings[0].IP;
    C.Port := Self.Server.Bindings[0].Port;
    C.Connect(OneSecond);

    KeepAliveSocket(C, true);
    Check(SocketUsesKeepAlive(C), 'Socket not set to use keep-alive');

    KeepAliveSocket(C, false);
    Check(not SocketUsesKeepAlive(C), 'Socket set to use keep-alive');

    KeepAliveSocket(C, true);
    Check(SocketUsesKeepAlive(C), 'Socket not set to use keep-alive');
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('Indy utility function tests', Suite);
end.
