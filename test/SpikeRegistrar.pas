{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SpikeRegistrar;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdObservable, IdSipCore, IdSipLocator,
  IdSipMessage, IdSipMockBindingDatabase, IdSipMockLocator,IdSipRegistration,
  IdSipTransaction, IdSipTransport, IdTimerQueue, StdCtrls, SyncObjs, SysUtils;

type
  TrnidSpikeRegistrar = class(TForm,
                              IIdObserver,
                              IIdSipTransportListener,
                              IIdSipTransportSendingListener)
    Log: TMemo;
    Panel1: TPanel;
    Port: TEdit;
    Splitter1: TSplitter;
    Contacts: TMemo;
    Label1: TLabel;
    procedure PortChange(Sender: TObject);
  private
    DB:         TIdSipMockBindingDatabase;
    Dispatcher: TIdSipTransactionDispatcher;
    Locator:    TIdSipMockLocator;
    Lock:       TCriticalSection;
    Timer:      TIdTimerQueue;
    Transport:  TIdSipTransport;
    UA:         TIdSipRegistrar;

    procedure LogMessage(Msg: TIdSipMessage);
    procedure OnChanged(Observed: TObject);
    procedure OnException(E: Exception;
                          const Reason: String);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpikeRegistrar: TrnidSpikeRegistrar;

implementation

{$R *.dfm}

uses
  IdStack, IdSipConsts, IdSocketHandle;

const
  LocalHostName = '127.0.0.1';

//******************************************************************************
//* TrnidSpikeRegistrar                                                        *
//******************************************************************************
//* TrnidSpikeRegistrar Public methods *****************************************

constructor TrnidSpikeRegistrar.Create(AOwner: TComponent);
var
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.Lock := TCriticalSection.Create;
  Self.Locator := TIdSipMockLocator.Create;
  Self.Timer := TIdThreadedTimerQueue.Create(false);

  Self.DB := TIdSipMockBindingDatabase.Create;

  Self.Transport := TIdSipUDPTransport.Create;
  if (GStack.LocalAddress <> LocalHostName) then
    Self.Transport.HostName := GStack.LocalAddress
  else
    Self.Transport.HostName := LocalHostName;
  Self.Transport.AddBinding(Self.Transport.HostName, StrToInt(Self.Port.Text));
  Self.Transport.AddTransportListener(Self);
  Self.Transport.AddTransportSendingListener(Self);
  Self.Transport.Start;

  Self.Dispatcher := TIdSipTransactionDispatcher.Create(Self.Timer, Self.Locator);
  Self.Dispatcher.AddTransport(Self.Transport);
  Self.UA := TIdSipRegistrar.Create;
  Self.UA.Dispatcher    := Self.Dispatcher;
  Self.UA.BindingDB     := Self.DB;
  Self.UA.HostName      := 'wsfrank';
  Self.UA.Timer         := Self.Timer;
  Self.UA.UserAgentName := 'Frank''s Registration Spike';

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@'
                   + Self.Transport.HostName + ':'
                   + Self.Port.Text;
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
end;

destructor TrnidSpikeRegistrar.Destroy;
begin
  Self.Timer.Terminate;
  Self.UA.Free;
  Self.Locator.Free;
  Self.Lock.Free;
  Self.DB.Free;

  inherited Destroy;
end;

//* TrnidSpikeRegistrar Private methods ****************************************

procedure TrnidSpikeRegistrar.LogMessage(Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegistrar.OnChanged(Observed: TObject);
var
  I: Integer;
begin
  Self.Lock.Acquire;
  try
    Self.Contacts.Lines.BeginUpdate;
    try
      Self.Contacts.Lines.Clear;
      for I := 0 to Self.DB.BindingCount - 1 do
        Self.Contacts.Lines.Add(Self.DB.Bindings[I].AddressOfRecord + ': ' + Self.DB.Bindings[I].Uri)
    finally
      Self.Contacts.Lines.EndUpdate;
    end;
  finally
    Self.Lock.Release;
  end;
end;

procedure TrnidSpikeRegistrar.OnException(E: Exception;
                                         const Reason: String);
begin
  Self.Log.Lines.Add('---- Exception ' + E.ClassName
                   + ' raised: ' + E.Message
                   + ' because: ''' + Reason + ''' ----');
end;

procedure TrnidSpikeRegistrar.OnReceiveRequest(Request: TIdSipRequest;
                                               Receiver: TIdSipTransport;
                                               Source: TIdSipConnectionBindings);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegistrar.OnReceiveResponse(Response: TIdSipResponse;
                                                Receiver: TIdSipTransport;
                                                Source: TIdSipConnectionBindings);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegistrar.OnRejectedMessage(const Msg: String;
                                                const Reason: String);
begin
  Self.Log.Lines.Add('----REJECTED MESSAGE: ' + Reason + '----');
  Self.Log.Lines.Add(Msg);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegistrar.OnSendRequest(Request: TIdSipRequest;
                                            Sender: TIdSipTransport;
                                            Destination: TIdSipLocation);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegistrar.OnSendResponse(Response: TIdSipResponse;
                                             Sender: TIdSipTransport;
                                             Destination: TIdSipLocation);
begin
  Self.LogMessage(Response);
end;

//* TrnidSpikeRegistrar Published methods **************************************

procedure TrnidSpikeRegistrar.PortChange(Sender: TObject);
begin
  Self.Transport.Stop;
  Self.Transport.ClearBindings;
  Self.Transport.AddBinding(Self.Transport.HostName, StrToInt(Self.Port.Text));
  Self.Transport.Start;
end;

end.
