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
  IdSipLocation, IdSipMessage, IdSipTransport, IdSocketHandle, SysUtils;

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

    procedure AddIndyStyleDefaultBinding;
    function  CreateFakeBinding: TIdSipConnectionBindings;
    procedure DispatchRequest(R: TidSipRequest;
                              Dest: TIdSipLocation);
    procedure DispatchResponse(R: TidSipResponse;
                               Dest: TIdSipLocation);
    function  FindTransport(const TransportType: String;
                            const Address: String;
                                  Port: Cardinal): TIdSipMockTransport;
    procedure Log(Msg: String;
                  Direction: TIdMessageDirection);
    procedure ScheduleException(Msg: TIdSipMessage);
    procedure SetWriteLog(const Value: Boolean);
    function  TransportAt(Index: Integer): TIdSipMockTransport;
  protected
    function  GetBindings: TIdSocketHandles; override;
    procedure SendRequest(R: TIdSipRequest;
                          Dest: TIdSipLocation); override;
    procedure SendResponse(R: TIdSipResponse;
                           Dest: TIdSipLocation); override;
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
                            Peer: TIdSipConnectionBindings); overload;
    procedure FireOnRejectedMessage(Msg: TIdSipMessage;
                                    const Reason: String);
    procedure FireOnResponse(R: TIdSipResponse); overload;
    procedure FireOnResponse(R: TIdSipResponse;
                             Peer: TIdSipConnectionBindings); overload;
    function  IsReliable: Boolean; override;
    function  IsRunning: Boolean; override;
    function  LastRequest: TIdSipRequest;
    function  LastResponse: TIdSipResponse;
    function  PeerIP: String;
    function  PeerPort: Integer;
    procedure RaiseException(E: ExceptClass);
    procedure ReceiveRequest(Request: TIdSipRequest;
                             ReceivedFrom: TIdSipConnectionBindings); override;
    procedure ReceiveResponse(Response: TIdSipResponse;
                              ReceivedFrom: TIdSipConnectionBindings); override;
    function  RequestAt(Index: Integer): TIdSipRequest;
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
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

  TIdSipMockSctpTransport = class(TIdSipMockTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTcpTransport = class(TIdSipMockTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTlsTransport = class(TIdSipMockTransport)
  public
    class function GetTransportType: String; override;
    class function MockedClass: TIdSipTransportClass; override;
  end;

  TIdSipMockTlsOverSctpTransport = class(TIdSipMockTransport)
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
  FakeBinding: TIdSipConnectionBindings;
begin
  FakeBinding := Self.CreateFakeBinding;
  try
    Self.FireOnRequest(R, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnRequest(R: TIdSipRequest;
                                            Peer: TIdSipConnectionBindings);
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
  FakeBinding:   TIdSipConnectionBindings;
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
  FakeBinding: TIdSipConnectionBindings;
begin
  FakeBinding := Self.CreateFakeBinding;
  try
    Self.FireOnResponse(R, FakeBinding);
  finally
    FakeBinding.Free;
  end;
end;

procedure TIdSipMockTransport.FireOnResponse(R: TIdSipResponse;
                                             Peer: TIdSipConnectionBindings);
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
                                             ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Log(Request.AsString, dirIn);
  Self.fRequests.AddCopy(Request);

  inherited ReceiveRequest(Request, ReceivedFrom);
end;

procedure TIdSipMockTransport.ReceiveResponse(Response: TIdSipResponse;
                                              ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Log(Response.AsString, dirIn);
  Self.fResponses.AddCopy(Response);

  inherited ReceiveResponse(Response, ReceivedFrom);
end;

function TIdSipMockTransport.RequestAt(Index: Integer): TIdSipRequest;
begin
  Result := Self.fRequests.Items[Index];
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
  if (Self.Bindings.Count = 0) then
    Self.AddIndyStyleDefaultBinding;

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

procedure TIdSipMockTransport.SendRequest(R: TIdSipRequest;
                                          Dest: TIdSipLocation);
begin
  inherited SendRequest(R, Dest);

  Self.Log(R.AsString, dirOut);

  if R.IsAck then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else begin
    Self.fRequests.AddCopy(R);
    Inc(Self.fSentRequestCount);
  end;

  if Assigned(Self.FailWith) then begin
    Self.ScheduleException(R);

    // Never autodispatch a message that's "failed".
    Exit;
  end;

  if Self.AutoDispatch then
    Self.DispatchRequest(R, Dest);
end;

procedure TIdSipMockTransport.SendResponse(R: TIdSipResponse;
                                           Dest: TIdSipLocation);
begin
  Self.NotifyOfSentResponse(R, Dest);

  Self.Log(R.AsString, dirOut);
  Self.fResponses.AddCopy(R);

  Inc(Self.fSentResponseCount);

  if Assigned(Self.FailWith) then begin
    Self.ScheduleException(R);

    // Never autodispatch a message that's "failed".
    Exit;
  end;

  if Self.AutoDispatch then
    Self.DispatchResponse(R, Dest);
end;

//* TIdSipMockTransport Private methods ****************************************

procedure TIdSipMockTransport.AddIndyStyleDefaultBinding;
var
  Binding: TIdSocketHandle;
begin
  Binding := Self.Bindings.Add;
  Binding.IP   := '127.0.01';
  Binding.Port := Self.DefaultPort;
end;

function TIdSipMockTransport.CreateFakeBinding: TIdSipConnectionBindings;
begin
  Result := TIdSipConnectionBindings.Create;

  Result.LocalIP   := Self.Bindings[0].IP;
  Result.LocalPort := Self.Bindings[0].Port;
  Result.PeerIP    := Self.PeerIP;
  Result.PeerPort  := Self.PeerPort;
  Result.Transport := Self.GetTransportType;
end;

procedure TIdSipMockTransport.DispatchRequest(R: TidSipRequest;
                                              Dest: TIdSipLocation);
var
  FakeBinding: TIdSipConnectionBindings;
  T:           TIdSipMockTransport;
begin
  T := Self.FindTransport(Dest.Transport, Dest.IPAddress, Dest.Port);

  if Assigned(T) then begin
    // FakeBinding represents the socket binding information that the remote
    // transport sees.
    FakeBinding := TIdSipConnectionBindings.Create;
    try
      FakeBinding.LocalIP   := T.Bindings[0].IP;
      FakeBinding.LocalPort := T.Bindings[0].Port;
      FakeBinding.PeerIP    := Self.Bindings[0].IP;
      FakeBinding.PeerPort  := Self.Bindings[0].Port;
      FakeBinding.Transport := T.GetTransportType;

      T.FireOnRequest(R, FakeBinding);
    finally
      FakeBinding.Free;
    end;
  end;
end;

procedure TIdSipMockTransport.DispatchResponse(R: TidSipResponse;
                                               Dest: TIdSipLocation);
var
  FakeBinding: TIdSipConnectionBindings;
  T:           TIdSipMockTransport;
begin
  T := Self.FindTransport(Dest.Transport, Dest.IPAddress, Dest.Port);

  if Assigned(T) then begin
    // FakeBinding represents the socket binding information that the remote
    // transport sees.
    FakeBinding := TIdSipConnectionBindings.Create;
    try
      FakeBinding.LocalIP   := T.Bindings[0].IP;
      FakeBinding.LocalPort := T.Bindings[0].Port;
      FakeBinding.PeerIP    := Self.Bindings[0].IP;
      FakeBinding.PeerPort  := Self.Bindings[0].Port;
      FakeBinding.Transport := T.GetTransportType;

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

procedure TIdSipMockTransport.ScheduleException(Msg: TIdSipMessage);
var
  Wait: TIdSipMessageExceptionWait;
begin
  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionType    := Self.FailWith;
  Wait.ExceptionMessage := 'Error injection';
  Wait.FailedMessage    := Msg;
  Wait.Reason           := 'TIdSipMockTransport.SendRequest (' + Self.FailWith.ClassName + ')';
  Wait.TransportID      := Self.ID;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
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
