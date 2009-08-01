{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdIndyUtils;

// This unit contains various Indy-interfacing utility functions.

interface

uses
  IdSocketHandle, IdTCPConnection, IdTcpServer, SysUtils;

type
  TIdTcpServerClass = class of TIdTcpServer;

function  BindingsToStr(Bindings: TIdSocketHandles): String;
function  BoolToSockOpt(B: Boolean): Integer;
function  CreateTcpServer(ServerType: TIdTcpServerClass): TIdTcpServer;
procedure KeepAliveSocket(C: TIdTCPConnection; KeepAlive: Boolean);
procedure RaiseSocketError(OriginalException: Exception; Bindings: TIdSocketHandles);

implementation

uses
  IdException, IdStackConsts, IdThreadMgr, IdThreadMgrDefault;

// ThreadMgr avoids a race condition. If you create a TIdTcpServer without
// giving it a ThreadMgr, it uses an ImplicitThreadMgr, a lazily created
// TIdThreadMgr. When the server is freed, it frees this implicit ThreadMgr.
// It's possible for the TIdListenerThread to then try release itself by calling
// Server.ThreadMgr.Release(LThread). This creates a new ThreadMgr and, because
// this new ThreadMgr has no reference to LThread, won't terminate and free it.
// When your application shuts down, IdThread.WaitAllTerminated will wait
// forever as GThreadCount will never reach zero. Symptom: the application looks
// like it's shut down, but you can see the process just sitting there.
var
  ThreadMgr: TIdThreadMgr;

//******************************************************************************
//* Unit Public functions & procedures                                         *
//******************************************************************************

function BindingsToStr(Bindings: TIdSocketHandles): String;
const
  Separator = ', ';
var
  I: Integer;
begin
  Result := '';

  if (Bindings.Count = 0) then begin
    Result := '*:' + IntToStr(Bindings.DefaultPort);
    Exit;
  end;

  for I := 0 to Bindings.Count - 1 do begin
    Result := Result + Bindings[I].IP + ':' + IntToStr(Bindings[I].Port) + Separator;
  end;

  Result := Copy(Result, 1, Length(Result) - Length(Separator));
end;

function BoolToSockOpt(B: Boolean): Integer;
begin
  if B then
    Result := Id_SO_True
  else
    Result := Id_SO_False;
end;

function CreateTcpServer(ServerType: TIdTcpServerClass): TIdTcpServer;
begin
  Result := ServerType.Create(nil);
  Result.ThreadMgr := ThreadMgr;
end;

procedure KeepAliveSocket(C: TIdTCPConnection; KeepAlive: Boolean);
var
  SetKeepalive: Integer;
begin
  if C.Connected then begin
    SetKeepalive := BoolToSockOpt(KeepAlive);
    C.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_KEEPALIVE, @SetKeepalive, Sizeof(SetKeepalive));
  end;
end;

procedure RaiseSocketError(OriginalException: Exception; Bindings: TIdSocketHandles);
const
  Msg = 'Could not open socket on one of (%s) (%s: %s)';
begin
  raise EIdSocketError.Create(Format(Msg, [BindingsToStr(Bindings), OriginalException.ClassName, OriginalException.Message]));
end;

initialization
  ThreadMgr := TIdThreadMgrDefault.Create(nil);
end.
