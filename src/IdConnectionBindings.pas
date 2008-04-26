{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdConnectionBindings;

interface

uses
  Classes, Contnrs;

type
  // I describe a socket. For connection oriented transports (TCP, say) I
  // describe the local and remote ip:ports; for connectionless transports
  // (UDP), I don't describe a connection, but I can tell you the sending
  // and receiving ip:ports.
  //
  // Bear in mind that I don't prescribe how you use the Transport property: a
  // SIP stack might populate it with well-known SIP transports ('UDP', 'TLS',
  // 'TLS-SCTP', etc.) while an SDP stream might use the protocol in the m
  // header ('RTP/AVP', 'vat', 'TCP', etc.).
  TIdConnectionBindings = class(TPersistent)
  private
    fLocalIP:   String;
    fLocalPort: Cardinal;
    fPeerIP:    String;
    fPeerPort:  Cardinal;
    fTransport: String;
  public
    constructor Create; overload;
    constructor Create(LocalIP: String; LocalPort: Cardinal; PeerIP: String; PeerPort: Cardinal; Transport: String); overload;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  Copy: TIdConnectionBindings;
    function  Equals(Other: TIdConnectionBindings): Boolean;

    property LocalIP:   String   read fLocalIP write fLocalIP;
    property LocalPort: Cardinal read fLocalPort write fLocalPort;
    property PeerIP:    String   read fPeerIP write fPeerIP;
    property PeerPort:  Cardinal read fPeerPort write fPeerPort;
    property Transport: String   read fTransport write fTransport;
  end;

  TIdConnectionBindingsSet = class(TPersistent)
  private
    List: TObjectList;

    function GetBindings(Index: Integer): TIdConnectionBindings;
    function IndexOfBinding(Binding: TIdConnectionBindings): Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(NewBinding: TIdConnectionBindings);
    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  AsStringWithPrefix(Prefix: String): String;
    procedure Clear;
    function  Count: Integer;
    function  HasBinding(Binding: TIdConnectionBindings): Boolean;
    function  IsEmpty: Boolean;
    procedure Remove(Binding: TIdConnectionBindings);

    property Bindings[Index: Integer]: TIdConnectionBindings read GetBindings; default;
  end;

// Miscellaneous constants
const
  BindingTuple = '(connection-bindings local-ip: %s local-port: %d peer-ip: %s peer-port: %d transport: %s)';
  TcpTransport = 'TCP';
  UdpTransport = 'UDP';

implementation

uses
  SysUtils;

const
  ItemNotFoundIndex = -1;

//******************************************************************************
//* TIdConnectionBindings                                                      *
//******************************************************************************
//* TIdConnectionBindings Public methods ***************************************

constructor TIdConnectionBindings.Create;
begin
  inherited Create;
end;

constructor TIdConnectionBindings.Create(LocalIP: String; LocalPort: Cardinal; PeerIP: String; PeerPort: Cardinal; Transport: String); 
begin
  inherited Create;

  Self.LocalIP   := LocalIP;
  Self.LocalPort := LocalPort;
  Self.PeerIP    := PeerIP;
  Self.PeerPort  := PeerPort;
  Self.Transport := Transport;
end;

procedure TIdConnectionBindings.Assign(Src: TPersistent);
var
  Other: TIdConnectionBindings;
begin
  if (Src is TIdConnectionBindings) then begin
    Other := Src as TIdConnectionBindings;

    Self.LocalIP   := Other.LocalIP;
    Self.LocalPort := Other.LocalPort;
    Self.PeerIP    := Other.PeerIP;
    Self.PeerPort  := Other.PeerPort;
    Self.Transport := Other.Transport;
  end
  else
    inherited Assign(Src);
end;

function TIdConnectionBindings.AsString: String;
begin
  Result := Format(BindingTuple, [Self.LocalIP,
                                  Self.LocalPort,
                                  Self.PeerIP,
                                  Self.PeerPort,
                                  Self.Transport]);
end;

function TIdConnectionBindings.Copy: TIdConnectionBindings;
begin
  Result := TIdConnectionBindings.Create;
  Result.Assign(Self);
end;

function TIdConnectionBindings.Equals(Other: TIdConnectionBindings): Boolean;
begin
  Result := (Self.LocalIP = Other.LocalIP)
        and (Self.LocalPort = Other.LocalPort)
        and (Self.PeerIP = Other.PeerIP)
        and (Self.PeerPort = Other.PeerPort)
        and (Self.Transport = Other.Transport);
end;

//******************************************************************************
//* TIdConnectionBindingsSet                                                   *
//******************************************************************************
//* TIdConnectionBindingsSet Public methods ************************************

constructor TIdConnectionBindingsSet.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdConnectionBindingsSet.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdConnectionBindingsSet.Add(NewBinding: TIdConnectionBindings);
begin
  if Self.HasBinding(NewBinding) then Exit;

  Self.List.Add(NewBinding.Copy);
end;

procedure TIdConnectionBindingsSet.Assign(Src: TPersistent);
var
  I:     Integer;
  Other: TIdConnectionBindingsSet;
begin
  if (Src is TIdConnectionBindingsSet) then begin
    Other := Src as TIdConnectionBindingsSet;

    Self.Clear;
    for I := 0 to Other.Count - 1 do
      Self.Add(Other[I]);
  end
  else
    inherited Assign(Src);
end;

function TIdConnectionBindingsSet.AsString: String;
begin
  Result := Self.AsStringWithPrefix('');
end;

function TIdConnectionBindingsSet.AsStringWithPrefix(Prefix: String): String;
const
  Terminator = #$D#$A;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count - 1 do
    Result := Result + Prefix + Self[I].AsString + Terminator;

  Result := Copy(Result, 1, Length(Result) - Length(Terminator));
end;

procedure TIdConnectionBindingsSet.Clear;
begin
  Self.List.Clear;
end;

function TIdConnectionBindingsSet.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdConnectionBindingsSet.HasBinding(Binding: TIdConnectionBindings): Boolean;
begin
  Result := Self.IndexOfBinding(Binding) <> ItemNotFoundIndex;
end;

function TIdConnectionBindingsSet.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

procedure TIdConnectionBindingsSet.Remove(Binding: TIdConnectionBindings);
var
  Index: Integer;
begin
  Index := Self.IndexOfBinding(Binding);

  if (Index <> ItemNotFoundIndex) then
    Self.List.Delete(Index);
end;

//* TIdConnectionBindingsSet Private methods ***********************************

function TIdConnectionBindingsSet.GetBindings(Index: Integer): TIdConnectionBindings;
begin
  Result := TIdConnectionBindings(Self.List[Index]);
end;

function TIdConnectionBindingsSet.IndexOfBinding(Binding: TIdConnectionBindings): Integer;
var
  I: Integer;
begin
  Result := ItemNotFoundIndex;
  for I := 0 to Self.Count - 1 do begin
    if Self[I].Equals(Binding) then begin
      Result := I;
      Break;
    end;
  end;
end;

end.
