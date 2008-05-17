{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockTransport;

interface

uses
  IdConnectionBindings, IdSipLocation, IdSipMessage, IdSipTransport,
  IdSocketHandle, SysUtils;

type
  TIdMessageDirection = (dirIn, dirOut);

  // The IsSecure function and TransportType properties deserve some
  // explanation. In every case except for a mock transport, the transport
  // subclass defines whether the transport is secure: no instance of
  // TIdUdpTransport can be secure, and no instance of TIdTlsTransport can be
  // insecure. It's very useful, in tests, to change mock transports to simulate
  // other transports: in some tests you want a UDP mock transport, and other
  // times you want a TLS transport.
  TIdSipMockTransport = class(TIdSipTransport)
  private
    fACKCount:          Cardinal;
    fAutoDispatch:      Boolean;
    fBindings:          TIdSocketHandles;
    fFailWith:          ExceptClass;
    fIsRunning:         Boolean;
    fLastACK:           TIdSipRequest;
    fRequests:          TIdSipRequestList;
    fResponses:         TIdSipResponseList;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fWriteLog:          Boolean;

    procedure CheckNoOtherMockTransportUsesBindings(Bindings: TIdSocketHandles);
    function  CreateFakeBinding: TIdConnectionBindings;
    procedure DispatchRequest(R: TidSipRequest;
                              Dest: TIdConnectionBindings);
    procedure DispatchResponse(R: TidSipResponse;
                               Dest: TIdConnectionBindings);
    function  FindTransport(const TransportType: String;
                            const Address: String;
                                  Port: Cardinal): TIdSipMockTransport;
    procedure InitialiseBinding(Binding: TIdConnectionBindings;
                                LocalBinding,
                                PeerBinding: TIdSocketHandle;
                                TransportType: String);
    procedure Log(Msg: String;
                  Direction: TIdMessageDirection);
    procedure PossiblyAutoDispatch(M: TIdSipMessage;
                                   Dest: TIdConnectionBindings);
    procedure RecordSentMessage(M: TIdSipMessage);
    procedure SetWriteLog(const Value: Boolean);
    function  TransportAt(Index: Integer): TIdSipMockTransport;
  protected
    function  GetBindings: TIdSocketHandles; override;
    procedure SendMessage(M: TIdSipMessage;
                          Dest: TIdConnectionBindings); override;
  public
    class function DefaultPort: Cardinal; override;
    class function GetTransportType: String; override;
    class function IsSecure: Boolean; override;
    class function MockedClass: TIdSipTransportClass; virtual;
    class function SrvPrefix: String; override;

    constructor Create; override;
    destructor  Destroy; override;

    procedure FireOnException(M: TIdSipMessage;
                              E: ExceptClass;
                              const ExceptionMessage: String;
                              const Reason: String);
    procedure FireOnRequest(R: TIdSipRequest); overload;
    procedure FireOnRequest(R: TIdSipRequest;
                            Peer: TIdConnectionBindings); overload;
    procedure FireOnRejectedMessage(Msg: TIdSipMessage;
                                    const Reason: String);
    procedure FireOnResponse(R: TIdSipResponse); overload;
    procedure FireOnResponse(R: TIdSipResponse;
                             Peer: TIdConnectionBindings); overload;
    function  IsMock: Boolean; override;
    function  IsReliable: Boolean; override;
    function  IsRunning: Boolean; override;
    function  LastRequest: TIdSipRequest;
    function  LastResponse: TIdSipResponse;
    function  PeerIP: String;
    function  PeerPort: Integer;
    procedure RaiseException(E: ExceptClass);
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdConnectionBindings); override;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              ReceivedFrom: TIdConnectionBindings); override;
    function  RequestAt(Index: Integer): TIdSipRequest;
    function  RequestCount: Integer;
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    function  ResponseCount: Integer;
    function  SecondLastRequest: TIdSipRequest;
    function  SecondLastResponse: TIdSipResponse;
    procedure Start; override;
    procedure Stop; override;
    function  ThirdLastRequest: TIdSipRequest;
    function  ThirdLastResponse: TIdSipResponse;

    property ACKCount:          Cardinal      read fACKCount;
    property AutoDispatch:      Boolean       read fAutoDispatch write fAutoDispatch;
    property FailWith:          ExceptClass   read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest read fLastACK;
    property SentRequestCount:  Cardinal      read fSentRequestCount;
    property SentResponseCount: Cardinal      read fSentResponseCount;
    property WriteLog:          Boolean       read fWriteLog write SetWriteLog;
  end;

  TIdSipMockConnectionOrientedTransport = class(TIdSipMockTransport)
  public
    procedure TriggerNotifyOfConnection(Connection: TIdConnectionBindings);
    procedure TriggerNotifyOfDisonnection(Connection: TIdConnectionBindings);
  end;

  TIdSipMockSctpTransport = class(TIdSipMockConnectionOrientedTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTcpTransport = class(TIdSipMockConnectionOrientedTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTlsTransport = class(TIdSipMockConnectionOrientedTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTlsOverSctpTransport = class(TIdSipMockConnectionOrientedTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockUdpTransport = class(TIdSipMockTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

const
  DebugLogName = 'MessageDump.log';

implementation

uses
  Classes, Contnrs, IdRTP, IdSipSctpTransport, IdSipTcpTransport,
  IdSipTlsTransport, IdSipUdpTransport, IdSipTlsOverSctpTransport, IdTimerQueue;

var
  GAllTransports: TObjectList;
  GLog:           TFileStream;
  GTransportType: String;

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

class function TIdSipMockTransport.MockedClass: TIdSipTransportClass;
begin
  raise Exception.Create(Self.ClassName + ' must override TIdSipMockTransport.MockedClass');
end;

class function TIdSipMockTransport.DefaultPort: Cardinal;
begin
  Result := Self.MockedClass.DefaultPort;
end;

class function TIdSipMockTransport.GetTransportType: String;
begin
  raise Exception.Create('Use a subclass of TIdSipMockTransport instead');
end;

class function TIdSipMockTransport.IsSecure: Boolean;
begin
  Result := Self.MockedClass.IsSecure;
end;

class function TIdSipMockTransport.SrvPrefix: String;
begin
  Result := Self.MockedClass.SrvPrefix;
end;

constructor TIdSipMockTransport.Create;
begin
  inherited Create;

  Self.ResetSentRequestCount;
  Self.fBindings  := TIdSocketHandles.Create(nil);
  Self.fLastACK   := TIdSipRequest.Create;
  Self.fRequests  := TIdSipRequestList.Create;
  Self.fResponses := TIdSipResponseList.Create;

  // All Indy servers instantiate with one binding.
  Self.Bindings.Add;

  GAllTransports.Add(Self);

  Self.AutoDispatch := false;
end;

destructor TIdSipMockTransport.Destroy;
begin
  GAllTransports.Remove(Self);

  Self.fResponses.Free;
  Self.fRequests.Free;
  Self.LastACK.Free;
  Self.Bindings.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransport.FireOnException(M: TIdSipMessage;
                                              E: ExceptClass;
                                              const ExceptionMessage: String;
                                              const Reason: String);
var
  Ex: Exception;
begin
  Ex := E.Create(ExceptionMessage);
  try
    Self.NotifyOfException(M, Ex, Reason);
  finally
    Ex.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnRequest(R: TIdSipRequest);
var
  FakeBinding: TIdConnectionBindings;
begin
  FakeBinding := Self.CreateFakeBinding;
  try
    Self.FireOnRequest(R, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnRequest(R: TIdSipRequest;
                                            Peer: TIdConnectionBindings);
var
  CopyOfMessage: TIdSipRequest;
begin
  // Simulate receiving a request from the network. Since we make no assumptions
  // about the code that invokes this method, we create a copy of the message to
  // pass up the stack to isolate the two chunks of code.

  CopyOfMessage := R.Copy as TIdSipRequest;
  try
    Self.ReceiveRequest(CopyOfMessage, Peer);
  finally
    CopyOfMessage.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnRejectedMessage(Msg: TIdSipMessage;
                                                    const Reason: String);
var
  CopyOfMessage: TIdSipMessage;
  FakeBinding:   TIdConnectionBindings;
begin
  Self.Log(Msg.AsString, dirIn);

  if Msg.IsRequest then
    Self.fRequests.AddCopy(Msg as TIdSipRequest)
  else
    Self.fResponses.AddCopy(Msg as TIdSipResponse);

  CopyOfMessage := Msg.Copy;
  try
    FakeBinding := Self.CreateFakeBinding;
    try
      Self.NotifyOfRejectedMessage(CopyOfMessage.AsString,
                                   Reason,
                                   FakeBinding);
    finally
      FakeBinding.Free;
    end;
  finally
    CopyOfMessage.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnResponse(R: TIdSipResponse);
var
  FakeBinding: TIdConnectionBindings;
begin
  FakeBinding := Self.CreateFakeBinding;
  try
    Self.FireOnResponse(R, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnResponse(R: TIdSipResponse;
                                             Peer: TIdConnectionBindings);
var
  CopyOfMessage: TIdSipResponse;
begin
  // Simulate receiving a response from the network. Since we make no
  // assumptions about the code that invokes this method, we create a copy of
  // the message to pass up the stack to isolate the two chunks of code.

  CopyOfMessage := R.Copy as TIdSipResponse;
  try
    Self.ReceiveResponse(CopyOfMessage, Peer);
  finally
    CopyOfMessage.Free;
  end;
end;

function TIdSipMockTransport.IsMock: Boolean;
begin
  Result := true;
end;

function TIdSipMockTransport.IsReliable: Boolean;
begin
  Result := Self.GetTransportType <> UdpTransport;
end;

function TIdSipMockTransport.IsRunning: Boolean;
begin
  Result := Self.fIsRunning;
end;

function TIdSipMockTransport.LastRequest: TIdSipRequest;
begin
  Result := Self.fRequests.Last;
end;

function TIdSipMockTransport.LastResponse: TIdSipResponse;
begin
  Result := Self.fResponses.Last;
end;

function TIdSipMockTransport.PeerIP: String;
begin
  Result := '192.168.255.254';
end;

function TIdSipMockTransport.PeerPort: Integer;
begin
  Result := 5060;
end;

procedure TIdSipMockTransport.RaiseException(E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
end;

procedure TIdSipMockTransport.ReceiveRequest(Request: TIdSipRequest;
                                             ReceivedFrom: TIdConnectionBindings);
begin
  Self.Log(Request.AsString, dirIn);
  Self.fRequests.AddCopy(Request);

  inherited ReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipMockTransport.ReceiveResponse(Response: TIdSipResponse;
                                              ReceivedFrom: TIdConnectionBindings);
begin
  Self.Log(Response.AsString, dirIn);
  Self.fResponses.AddCopy(Response);

  inherited ReceiveResponse(Response, ReceivedFrom);
end;

function TIdSipMockTransport.RequestAt(Index: Integer): TIdSipRequest;
begin
  Result := Self.fRequests.Items[Index];
end;

function TIdSipMockTransport.RequestCount: Integer;
begin
  Result := Self.fRequests.Count;
end;

procedure TIdSipMockTransport.ResetACKCount;
begin
  Self.fACKCount := 0;
end;

procedure TIdSipMockTransport.ResetSentRequestCount;
begin
  Self.fSentRequestCount := 0;
end;

procedure TIdSipMockTransport.ResetSentResponseCount;
begin
  Self.fSentResponseCount := 0;
end;

function TIdSipMockTransport.ResponseCount: Integer;
begin
  Result := Self.fResponses.Count;
end;

function TIdSipMockTransport.SecondLastRequest: TIdSipRequest;
begin
  Result := Self.fRequests.SecondLast;
end;

function TIdSipMockTransport.SecondLastResponse: TIdSipResponse;
begin
  Result := Self.fResponses.SecondLast;
end;

procedure TIdSipMockTransport.Start;
begin
  Self.CheckNoOtherMockTransportUsesBindings(Self.Bindings);

//  if (Self.Bindings.Count = 0) then
//    Self.AddIndyStyleDefaultBinding;

  Self.fIsRunning := true;
end;

procedure TIdSipMockTransport.Stop;
begin
  Self.fIsRunning := false;
end;

function TIdSipMockTransport.ThirdLastRequest: TIdSipRequest;
begin
  Result := Self.fRequests.ThirdLast;
end;

function TIdSipMockTransport.ThirdLastResponse: TIdSipResponse;
begin
  Result := Self.fResponses.ThirdLast;
end;

//* TIdSipMockTransport Protected methods **************************************

function TIdSipMockTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.fBindings;
end;

procedure TIdSipMockTransport.SendMessage(M: TIdSipMessage;
                                          Dest: TIdConnectionBindings);
var
  E:              Exception;
  SendingBinding: TIdSocketHandle;
begin
  Assert(Self.BindingCount > 0,
         'This MockTransport has no bindings on which to send this message');

  Self.Log(M.AsString, dirOut);

  SendingBinding := Self.FindBinding(Dest);

  if not Assigned(SendingBinding) then begin
    // A Mock Transport will always appear to send messages from its first
    // binding.
    SendingBinding := Self.Bindings[0];
  end;

  Dest.LocalIP   := SendingBinding.IP;
  Dest.LocalPort := SendingBinding.Port;

  if M.LastHop.IsUnset then
    M.RewriteLocationHeaders(Dest);

  Self.RecordSentMessage(M);

  if Assigned(Self.FailWith) then begin
    E := Self.FailWith.Create('Invoked failure');
    try
      Self.NotifyOfException(M, E, 'Invoked failure');
    finally
      E.Free;
    end;
  end
  else begin
    // Never autodispatch a message that's "failed".
    Self.PossiblyAutoDispatch(M, Dest);
  end;
end;

//* TIdSipMockTransport Private methods ****************************************

procedure TIdSipMockTransport.CheckNoOtherMockTransportUsesBindings(Bindings: TIdSocketHandles);
var
  I: Integer;
  T: TIdSipTransport;
begin
  for I := 0 to Bindings.Count - 1 do begin
    T := TIdSipDebugTransportRegistry.TransportRunningOn(Self.GetTransportType, Bindings[I].IP, Bindings[I].Port);

    if Assigned(T) and (T <> Self) and (T.IsRunning) then
      raise Exception.Create('There''s already a MockTransport running on '
                           + Bindings[I].IP + ':' + IntToStr(Bindings[I].Port));
  end;
end;

function TIdSipMockTransport.CreateFakeBinding: TIdConnectionBindings;
begin
  Result := TIdConnectionBindings.Create;

  Result.LocalIP   := Self.Bindings[0].IP;
  Result.LocalPort := Self.Bindings[0].Port;
  Result.PeerIP    := Self.PeerIP;
  Result.PeerPort  := Self.PeerPort;
  Result.Transport := Self.GetTransportType;
end;

procedure TIdSipMockTransport.DispatchRequest(R: TidSipRequest;
                                              Dest: TIdConnectionBindings);
var
  FakeBinding: TIdConnectionBindings;
  T:           TIdSipMockTransport;
begin
  T := Self.FindTransport(Dest.Transport, Dest.PeerIP, Dest.PeerPort);

  if Assigned(T) then begin
    // FakeBinding represents the socket binding information that the remote
    // transport sees.
    FakeBinding := TIdConnectionBindings.Create;
    try
      Self.InitialiseBinding(FakeBinding,
                             T.Bindings[0],
                             Self.Bindings[0],
                             T.GetTransportType);

      T.FireOnRequest(R, FakeBinding);
    finally
      FakeBinding.Free;
    end;
  end;
end;

procedure TIdSipMockTransport.DispatchResponse(R: TidSipResponse;
                                               Dest: TIdConnectionBindings);
var
  FakeBinding: TIdConnectionBindings;
  T:           TIdSipMockTransport;
begin
  T := Self.FindTransport(Dest.Transport, Dest.PeerIP, Dest.PeerPort);

  if Assigned(T) then begin
    // FakeBinding represents the socket binding information that the remote
    // transport sees.
    FakeBinding := TIdConnectionBindings.Create;
    try
      Self.InitialiseBinding(FakeBinding,
                             T.Bindings[0],
                             Self.Bindings[0],
                             T.GetTransportType);

      T.FireOnResponse(R, FakeBinding);
    finally
      FakeBinding.Free;
    end;
  end;
end;

function TIdSipMockTransport.FindTransport(const TransportType: String;
                                           const Address: String;
                                                 Port: Cardinal): TIdSipMockTransport;
var
  I: Integer;
begin
  Result := nil;

  I := 0;

  while (I < GAllTransports.Count) and not Assigned(Result) do
    if (Self.TransportAt(I).GetTransportType = TransportType)
      and Self.TransportAt(I).HasBinding(Address, Port) then
      Result := Self.TransportAt(I)
    else
      Inc(I);
end;

procedure TIdSipMockTransport.InitialiseBinding(Binding: TIdConnectionBindings;
                                                LocalBinding,
                                                PeerBinding: TIdSocketHandle;
                                                TransportType: String);
begin
  Binding.LocalIP   := LocalBinding.IP;
  Binding.LocalPort := LocalBinding.Port;
  Binding.PeerIP    := PeerBinding.IP;
  Binding.PeerPort  := PeerBinding.Port;
  Binding.Transport := TransportType;
end;

procedure TIdSipMockTransport.Log(Msg: String;
                                  Direction: TIdMessageDirection);
var
  Date: String;
begin
  if not Self.WriteLog then Exit;

  case Direction of
    dirIn:  Date := '<<<';
    dirOut: Date := '>>>';
  end;

  Date := Date + ' ' + FormatDateTime('yyyy/mm/dd hh:mm:ss.zzz', Now) + #13#10;

  WriteString(GLog, Date);
  WriteString(GLog, Msg);
  WriteString(GLog, #13#10);
end;

procedure TIdSipMockTransport.PossiblyAutoDispatch(M: TIdSipMessage;
                                                   Dest: TIdConnectionBindings);
begin
  if Self.AutoDispatch then begin
    if M.IsRequest then
      Self.DispatchRequest(M as TIdSipRequest, Dest)
    else
      Self.DispatchResponse(M as TIdSipResponse, Dest);
  end;
end;

procedure TIdSipMockTransport.RecordSentMessage(M: TIdSipMessage);
begin
  if M.IsAck then begin
    Self.LastACK.Assign(M);
    Inc(Self.fACKCount)
  end
  else if M.IsRequest then begin
    Self.fRequests.AddCopy(M as TIdSipRequest);
    Inc(Self.fSentRequestCount);
  end
  else if M.IsResponse then begin
    Self.fResponses.AddCopy(M as TIdSipResponse);
    Inc(Self.fSentResponseCount);
  end;
end;

procedure TIdSipMockTransport.SetWriteLog(const Value: Boolean);
begin
  if Value and not Assigned(GLog) then
    GLog := TFileStream.Create(DebugLogName,
                               fmCreate or fmShareDenyWrite);

  Self.fWriteLog := Value;
end;

function TIdSipMockTransport.TransportAt(Index: Integer): TIdSipMockTransport;
begin
  Result := GAllTransports[Index] as TIdSipMockTransport;
end;

//******************************************************************************
//* TIdSipMockConnectionOrientedTransport                                      *
//******************************************************************************
//* TIdSipMockConnectionOrientedTransport Public methods ***********************

procedure TIdSipMockConnectionOrientedTransport.TriggerNotifyOfConnection(Connection: TIdConnectionBindings);
begin
  Self.NotifyOfConnection(Connection);
end;

procedure TIdSipMockConnectionOrientedTransport.TriggerNotifyOfDisonnection(Connection: TIdConnectionBindings);
begin
  Self.NotifyOfDisconnection(Connection);
end;

//******************************************************************************
//* TIdSipMockSctpTransport                                                    *
//******************************************************************************
//* TIdSipMockSctpTransport Public methods *************************************

class function TIdSipMockSctpTransport.GetTransportType: String;
begin
  Result := SctpTransport;
end;

class function TIdSipMockSctpTransport.MockedClass: TIdSipTransportClass;
begin
  Result := TIdSipSctpTransport;
end;

//******************************************************************************
//* TIdSipMockTcpTransport                                                     *
//******************************************************************************
//* TIdSipMockTcpTransport Public methods **************************************

class function TIdSipMockTcpTransport.GetTransportType: String;
begin
  Result := TcpTransport;
end;

class function TIdSipMockTcpTransport.MockedClass: TIdSipTransportClass;
begin
  Result := TIdSipTcpTransport;
end;

//******************************************************************************
//* TIdSipMockTlsTransport                                                     *
//******************************************************************************
//* TIdSipMockTlsTransport Public methods **************************************

class function TIdSipMockTlsTransport.GetTransportType: String;
begin
  Result := TlsTransport;
end;

class function TIdSipMockTlsTransport.MockedClass: TIdSipTransportClass;
begin
  Result := TIdSipTlsTransport;
end;

//******************************************************************************
//* TIdSipMockTlsOverSctpTransport                                             *
//******************************************************************************
//* TIdSipMockTlsOverSctpTransport Public methods ******************************

class function TIdSipMockTlsOverSctpTransport.GetTransportType: String;
begin
  Result := TlsOverSctpTransport;
end;

class function TIdSipMockTlsOverSctpTransport.MockedClass: TIdSipTransportClass;
begin
  Result := TIdSipTlsOverSctpTransport;
end;

//******************************************************************************
//* TIdSipMockUdpTransport                                                     *
//******************************************************************************
//* TIdSipMockUdpTransport Public methods **************************************

class function TIdSipMockUdpTransport.GetTransportType: String;
begin
  Result := UdpTransport;
end;

class function TIdSipMockUdpTransport.MockedClass: TIdSipTransportClass;
begin
  Result := TIdSipUdpTransport;
end;

initialization
  GAllTransports := TObjectList.Create(false);
  GTransportType := UdpTransport;
finalization
// These objects are purely memory-based, so it's safe not to free them here.
// Still, perhaps we need to review this methodology. How else do we get
// something like class variables?
//  GLog.Free;
//  GAllTransports.Free;
end.
