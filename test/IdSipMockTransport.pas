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
  IdSipMessage, IdSipTransport, IdSocketHandle, SysUtils;

type
  TIdMessageDirection = (dirIn, dirOut);

  TIdSipMockTransport = class(TIdSipTransport)
  private
    fACKCount:          Cardinal;
    fAddress:           String;
    fBindings:          TIdSocketHandles;
    fFailWith:          ExceptClass;
    fLastACK:           TIdSipRequest;
    fRequests:          TIdSipRequestList;
    fResponses:         TIdSipResponseList;
    fPort:              Cardinal;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fTransportType:     TIdSipTransportType;
    fWriteLog:          Boolean;

    procedure DispatchRequest(R: TidSipRequest);
    procedure DispatchResponse(R: TidSipResponse);
    function  FindTransport(const Host: String;
                                  Port: Cardinal): TIdSipMockTransport;
    procedure Log(Msg: String;
                  Direction: TIdMessageDirection);
    procedure SetWriteLog(const Value: Boolean);
    function  TransportAt(Index: Integer): TIdSipMockTransport;
  protected
    procedure ChangeBinding(const Address: String; Port: Cardinal); override;
    function  GetAddress: String; override;
    function  GetBindings: TIdSocketHandles; override;
    function  GetPort: Cardinal; override;
    procedure SendRequest(R: TIdSipRequest); override;
    procedure SendResponse(R: TIdSipResponse); override;
    function  SentByIsRecognised(Via: TIdSipViaHeader): Boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure FireOnRequest(R: TIdSipRequest);
    procedure FireOnResponse(R: TIdSipResponse);
    function  GetTransportType: TIdSipTransportType; override;
    function  IsReliable: Boolean; override;
    function  IsSecure: Boolean; override;
    function  LastRequest: TIdSipRequest;
    function  LastResponse: TIdSipResponse;
    procedure RaiseException(E: ExceptClass);
    function  RequestAt(Index: Integer): TIdSipRequest;
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    function  SecondLastRequest: TIdSipRequest;
    function  SecondLastResponse: TIdSipResponse;
    procedure Start; override;
    procedure Stop; override;
    function  ThirdLastRequest: TIdSipRequest;

    property ACKCount:          Cardinal            read fACKCount;
    property FailWith:          ExceptClass         read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest       read fLastACK;
    property SentRequestCount:  Cardinal            read fSentRequestCount;
    property SentResponseCount: Cardinal            read fSentResponseCount;
    property TransportType:     TIdSipTransportType read fTransportType write fTransportType;
    property WriteLog:          Boolean             read fWriteLog write SetWriteLog;
  end;

const
  DebugLogName = 'MessageDump.log';

implementation

uses
  Classes, Contnrs, IdRTP;

var
  GAllTransports: TObjectList;
  GLog:           TFileStream;

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

constructor TIdSipMockTransport.Create;
begin
  inherited Create;

  Self.ResetSentRequestCount;
  Self.fBindings    := TIdSocketHandles.Create(nil);
  Self.fLastACK     := TIdSipRequest.Create;
  Self.fRequests    := TIdSipRequestList.Create;
  Self.fResponses   := TIdSipResponseList.Create;

  GAllTransports.Add(Self);
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

procedure TIdSipMockTransport.FireOnRequest(R: TIdSipRequest);
begin
  Self.Log(R.AsString, dirIn);

  Self.fRequests.AddCopy(R);

  Self.NotifyTransportListeners(R);
end;

procedure TIdSipMockTransport.FireOnResponse(R: TIdSipResponse);
begin
  Self.Log(R.AsString, dirIn);

  Self.fResponses.AddCopy(R);

  Self.NotifyTransportListeners(R);
end;

function TIdSipMockTransport.GetTransportType: TIdSipTransportType;
begin
  Result := Self.TransportType;
end;

function TIdSipMockTransport.IsReliable: Boolean;
begin
  Result := Self.TransportType <> sttUDP;
end;

function TIdSipMockTransport.IsSecure: Boolean;
begin
  Result := Self.TransportType = sttTLS;
end;

function TIdSipMockTransport.LastRequest: TIdSipRequest;
begin
  Result := Self.fRequests.Last;
end;

function TIdSipMockTransport.LastResponse: TIdSipResponse;
begin
  Result := Self.fResponses.Last;
end;

procedure TIdSipMockTransport.RaiseException(E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
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
end;

procedure TIdSipMockTransport.Stop;
begin
end;

function TIdSipMockTransport.ThirdLastRequest: TIdSipRequest;
begin
  Result := Self.fRequests.ThirdLast;
end;

//* TIdSipMockTransport Protected methods **************************************

procedure TIdSipMockTransport.ChangeBinding(const Address: String; Port: Cardinal);
begin
  Self.fAddress := Address;
  Self.fPort    := Port;
end;

function TIdSipMockTransport.GetAddress: String;
begin
  Result := Self.fAddress;
end;

function TIdSipMockTransport.GetBindings: TIdSocketHandles;
begin
  Result := Self.fBindings;
end;

function TIdSipMockTransport.GetPort: Cardinal;
begin
  Result := Self.fPort;
end;

procedure TIdSipMockTransport.SendRequest(R: TIdSipRequest);
begin
  inherited SendRequest(R);

  Self.Log(R.AsString, dirOut);

  if R.IsAck then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else begin
    Self.fRequests.AddCopy(R);
    Inc(Self.fSentRequestCount);
  end;

  if Assigned(Self.FailWith) then
    raise EIdSipTransport.Create(Self,
                                 R,
                                 'TIdSipMockTransport.SendRequest ('
                               + Self.FailWith.ClassName + ')');

  Self.DispatchRequest(R);
end;

procedure TIdSipMockTransport.SendResponse(R: TIdSipResponse);
begin
  Self.Log(R.AsString, dirOut);
  Self.fResponses.AddCopy(R);

  if Assigned(Self.FailWith) then
    raise EIdSipTransport.Create(Self,
                                 R,
                                 'TIdSipMockTransport.SendResponse ('
                               + Self.FailWith.ClassName + ')');

  Inc(Self.fSentResponseCount);

  // We call inherited at the end because we want to update our state first,
  // so listeners get a true idea of, for instance, SentResponseCount.
  inherited SendResponse(R);

  Self.DispatchResponse(R);
end;

function TIdSipMockTransport.SentByIsRecognised(Via: TIdSipViaHeader): Boolean;
begin
  Result := true;
end;

//* TIdSipMockTransport Private methods ****************************************

procedure TIdSipMockTransport.DispatchRequest(R: TidSipRequest);
var
  T: TIdSipMockTransport;
begin
  T := Self.FindTransport(R.RequestUri.Host, R.RequestUri.Port);

  if Assigned(T) then
    T.FireOnRequest(R);
end;

procedure TIdSipMockTransport.DispatchResponse(R: TidSipResponse);
var
  T: TIdSipMockTransport;
begin
  T := Self.FindTransport(R.LastHop.SentBy, R.LastHop.Port);

  if Assigned(T) then
    T.FireOnResponse(R);
end;

function TIdSipMockTransport.FindTransport(const Host: String;
                                                 Port: Cardinal): TIdSipMockTransport;
  function NameMatches(Transport: TIdSipMockTransport; const Host: String): Boolean;
  begin
    Result := IsEqual(Transport.HostName, Host)
           or IsEqual(Transport.Address, Host);
  end;
var
  I: Integer;
begin
  Result := nil;

  I := 0;
  while (I < GAllTransports.Count) and not Assigned(Result) do
    if NameMatches(Self.TransportAt(I), Host)
       and (Self.TransportAt(I).Port = Port) then
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

initialization
  GAllTransports := TObjectList.Create(false);
finalization
  GLog.Free;
  GAllTransports.Free;
end.
