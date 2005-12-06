{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SpikeRegister;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipIndyLocator, IdSipLocator,
  IdSipMessage, IdSipRegistration, IdSipTransaction, IdSipTransport,
  IdSipUserAgent, IdTimerQueue, StdCtrls, SyncObjs, SysUtils;

type
  TrnidSpikeRegister = class(TForm,
                             IIdSipRegistrationListener,
                             IIdSipTransportListener,
                             IIdSipTransportSendingListener)
    Log: TMemo;
    Panel1: TPanel;
    Register: TButton;
    Query: TButton;
    Unregister: TButton;
    Registrar: TEdit;
    Splitter1: TSplitter;
    Contacts: TMemo;
    procedure QueryClick(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure UnregisterClick(Sender: TObject);
  private
    Dispatcher:  TIdSipTransactionDispatcher;
    Locator:     TIdSipIndyLocator;
    Lock:        TCriticalSection;
    RunningPort: Cardinal;
    Timer:       TIdTimerQueue;
    Transport:   TIdSipTransport;
    UA:          TIdSipUserAgent;

    procedure LogMessage(Msg: TIdSipMessage);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        Response: TIdSipResponse);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport;
                               Source: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport;
                                Source: TIdSipConnectionBindings);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport;
                            Destination: TIdSipLocation);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport;
                             Destination: TIdSipLocation);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure RefreshContacts(Bindings: TIdSipContacts);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpikeRegister: TrnidSpikeRegister;

implementation

{$R *.dfm}

uses
  IdStack, IdSipConsts, IdSocketHandle;

const
  LocalHostName = '127.0.0.1';

//******************************************************************************
//* TrnidSpikeRegister                                                         *
//******************************************************************************
//* TrnidSpikeRegister Public methods ******************************************

constructor TrnidSpikeRegister.Create(AOwner: TComponent);
var
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.Lock := TCriticalSection.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);

  Self.Locator := TIdSipIndyLocator.Create;
  Self.Locator.NameServer := '127.0.0.1'; // bogus value!
  Self.Locator.Port       := 53;

  Self.Transport := TIdSipUDPTransport.Create;
  if (GStack.LocalAddress <> LocalHostName) then
    Self.Transport.HostName := GStack.LocalAddress
  else
    Self.Transport.HostName := LocalHostName;
  Self.Transport.Address := Self.Transport.HostName;

  Self.Transport.AddTransportListener(Self);
  Self.Transport.AddTransportSendingListener(Self);

  Self.Dispatcher := TIdSipTransactionDispatcher.Create(Self.Timer, Self.Locator);
  Self.Dispatcher.AddTransport(Self.Transport);
  Self.UA := TIdSipUserAgent.Create;
  Self.UA.Dispatcher := Self.Dispatcher;
  Self.UA.HostName := 'wsfrank';
  Self.UA.UserAgentName := 'Frank''s Registration Spike';

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@'
                   + Self.Transport.HostName + ':'
                   + IntToStr(Self.Transport.Port);
    Self.UA.Contact := Contact;
  finally
    Contact.Free;
  end;

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'sip:franks@' + Self.Transport.HostName;
    Self.UA.From := From;
  finally
    From.Free;
  end;

  Self.RunningPort := Self.Transport.DefaultPort;
end;

destructor TrnidSpikeRegister.Destroy;
begin
  Self.Timer.Terminate;
  Self.UA.Free;
  Self.Lock.Free;
  Self.Locator.Free;

  inherited Destroy;
end;

//* TrnidSpikeRegister Private methods *****************************************

procedure TrnidSpikeRegister.LogMessage(Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegister.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Response: TIdSipResponse);
begin
end;                                                       

procedure TrnidSpikeRegister.OnException(E: Exception;
                                         const Reason: String);
begin
  Self.Log.Lines.Add('---- Exception ' + E.ClassName
                   + ' raised: ' + E.Message
                   + ' because: ''' + Reason + ''' ----');
end;

procedure TrnidSpikeRegister.OnNetworkFailure(Action: TIdSipAction;
                                              ErrorCode: Cardinal;
                                              const Reason: String);
begin
end;

procedure TrnidSpikeRegister.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                       CurrentBindings: TIdSipContacts;
                                       Response: TIdSipResponse);
begin
  Self.Log.Lines.Add('---- Registration action failed: ' + Response.Description + ' ----');
  Self.RefreshContacts(CurrentBindings);
  RegisterAgent.RemoveListener(Self);
end;

procedure TrnidSpikeRegister.OnReceiveRequest(Request: TIdSipRequest;
                                              Receiver: TIdSipTransport;
                                              Source: TIdSipConnectionBindings);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnReceiveResponse(Response: TIdSipResponse;
                                               Receiver: TIdSipTransport;
                                               Source: TIdSipConnectionBindings);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegister.OnRejectedMessage(const Msg: String;
                                               const Reason: String);
begin
  Self.Log.Lines.Add('----REJECTED MESSAGE: ' + Reason + '----');
  Self.Log.Lines.Add(Msg);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegister.OnSendRequest(Request: TIdSipRequest;
                                           Sender: TIdSipTransport;
                                           Destination: TIdSipLocation);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnSendResponse(Response: TIdSipResponse;
                                            Sender: TIdSipTransport;
                                            Destination: TIdSipLocation);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegister.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                       CurrentBindings: TIdSipContacts);
begin
  Self.Log.Lines.Add('---- Registration action succeeded ----');
  RegisterAgent.RemoveListener(Self);

  Self.RefreshContacts(CurrentBindings);
end;

procedure TrnidSpikeRegister.RefreshContacts(Bindings: TIdSipContacts);
begin
  Self.Lock.Acquire;
  try
    Self.Contacts.Lines.Clear;
    Bindings.First;

    while Bindings.HasNext do begin
      Self.Contacts.Lines.Add(Bindings.CurrentContact.AsAddressOfRecord);
      Bindings.Next;
    end;
  finally
    Self.Lock.Release;
  end;
end;

//* TrnidSpikeRegister Published methods ***************************************

procedure TrnidSpikeRegister.QueryClick(Sender: TObject);
var
  Q:            TIdSipOutboundRegistrationQuery;
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Q := Self.UA.RegisterModule.CurrentRegistrationWith(RegistrarUri);
    Q.AddListener(Self);
    Q.Send;
  finally
    RegistrarUri.Free;
  end;
end;

procedure TrnidSpikeRegister.RegisterClick(Sender: TObject);
var
  Reg:          TIdSipOutboundRegister;
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Reg := Self.UA.RegisterModule.RegisterWith(RegistrarUri);
    Reg.AddListener(Self);
    Reg.Send;
  finally
    RegistrarUri.Free;
  end;
end;

procedure TrnidSpikeRegister.UnregisterClick(Sender: TObject);
var
  Unreg:        TIdSipOutboundUnregister;
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Unreg := Self.UA.RegisterModule.UnregisterFrom(RegistrarUri);
    Unreg.AddListener(Self);
    Unreg.Send;
  finally
    RegistrarUri.Free;
  end;
end;

end.
