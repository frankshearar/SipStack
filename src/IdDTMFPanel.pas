unit IdDTMFPanel;

interface

uses
  Classes, ExtCtrls, IdRTP, IdRTPTimerQueue, IdSdp, IdSocketHandle, StdCtrls;

type
  TColourButton = class(TButton)
  public
    property Color;
  end;

  TIdDTMFButtonArray = array[DTMF0..DTMFFlash] of TColourButton;

  // Still needs: flashing of buttons; maybe playing of tones, too, on receipt
  // of DTMF
  TIdDTMFPanel = class(TPanel,
                       IIdRTPDataListener)
  private
    Buttons:          TIdDTMFButtonArray;
    ButtonHeight:     Integer;
    CurrentRowHeight: Integer;
    fProcessor:       TIdSDPPayloadProcessor;
    Timer:            TIdRTPTimerQueue;

    procedure AddRow(Buttons: array of TColourButton);
    function  CreateButton(Name: String;
                           Event: TNotifyEvent): TColourButton;
    procedure ResizeButtons;
    procedure DoOnResize(Sender: TObject);
    procedure Flash(Event: TIdRTPTelephoneEventPayload);
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdSocketHandle);
    procedure SendDTMF(Event: Byte);
    procedure SendDTMF0(Sender: TObject);
    procedure SendDTMF1(Sender: TObject);
    procedure SendDTMF2(Sender: TObject);
    procedure SendDTMF3(Sender: TObject);
    procedure SendDTMF4(Sender: TObject);
    procedure SendDTMF5(Sender: TObject);
    procedure SendDTMF6(Sender: TObject);
    procedure SendDTMF7(Sender: TObject);
    procedure SendDTMF8(Sender: TObject);
    procedure SendDTMF9(Sender: TObject);
    procedure SendDTMFA(Sender: TObject);
    procedure SendDTMFB(Sender: TObject);
    procedure SendDTMFC(Sender: TObject);
    procedure SendDTMFD(Sender: TObject);
    procedure SendDTMFFlash(Sender: TObject);
    procedure SendDTMFHash(Sender: TObject);
    procedure SendDTMFStar(Sender: TObject);
    procedure SetProcessor(Value: TIdSDPPayloadProcessor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property Processor: TIdSDPPayloadProcessor read fProcessor write SetProcessor;
  end;

implementation

uses
  Graphics, SysUtils;

//*****************************************************************************
//* TIdDTMFPanel                                                              *
//*****************************************************************************
//* TIdDTMFPanel Public methods ***********************************************

constructor TIdDTMFPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Timer := TIdRTPTimerQueue.Create(false);

  Self.ButtonHeight     := 25;
  Self.Height           := 120;
  Self.OnResize         := Self.DoOnResize;
  Self.Width            := 96;

  Self.Buttons[DTMF0]     := Self.CreateButton('0',     Self.SendDTMF0);
  Self.Buttons[DTMF1]     := Self.CreateButton('1',     Self.SendDTMF1);
  Self.Buttons[DTMF2]     := Self.CreateButton('2',     Self.SendDTMF2);
  Self.Buttons[DTMF3]     := Self.CreateButton('3',     Self.SendDTMF3);
  Self.Buttons[DTMF4]     := Self.CreateButton('4',     Self.SendDTMF4);
  Self.Buttons[DTMF5]     := Self.CreateButton('5',     Self.SendDTMF5);
  Self.Buttons[DTMF6]     := Self.CreateButton('6',     Self.SendDTMF6);
  Self.Buttons[DTMF7]     := Self.CreateButton('7',     Self.SendDTMF7);
  Self.Buttons[DTMF8]     := Self.CreateButton('8',     Self.SendDTMF8);
  Self.Buttons[DTMF9]     := Self.CreateButton('9',     Self.SendDTMF9);
  Self.Buttons[DTMFA]     := Self.CreateButton('A',     Self.SendDTMFA);
  Self.Buttons[DTMFB]     := Self.CreateButton('B',     Self.SendDTMFB);
  Self.Buttons[DTMFC]     := Self.CreateButton('C',     Self.SendDTMFC);
  Self.Buttons[DTMFD]     := Self.CreateButton('D',     Self.SendDTMFD);
  Self.Buttons[DTMFStar]  := Self.CreateButton('*',     Self.SendDTMFStar);
  Self.Buttons[DTMFHash]  := Self.CreateButton('#',     Self.SendDTMFHash);
  Self.Buttons[DTMFFlash] := Self.CreateButton('Flash', Self.SendDTMFFlash);

  Self.ResizeButtons;
end;

destructor TIdDTMFPanel.Destroy;
var
  I: Integer;
begin
  for I := Low(Self.Buttons) to High(Self.Buttons) do
    Self.Buttons[I].Free;

  Self.Timer.TerminateAndWaitFor;
  Self.Timer.Free;

  inherited Destroy;
end;

//* TIdDTMFPanel Private methods ***********************************************

procedure TIdDTMFPanel.AddRow(Buttons: array of TColourButton);
var
  I:      Integer;
  XCoord: Integer;
  Width:  Integer;
begin
  XCoord := 0;
  Width := Self.Width div Length(Buttons) + 1;
  for I := Low(Buttons) to High(Buttons) do begin
    Buttons[I].Height := Self.ButtonHeight;
    Buttons[I].Left   := XCoord;
    Buttons[I].Parent := Self; // Yes, yes, this is wasteful
    Buttons[I].Top    := Self.CurrentRowHeight;
    Buttons[I].Width  := Width;
    Inc(XCoord, Buttons[I].Width - 1);
  end;

  // Adjust the last button to take up all remaining horizontal space
  Buttons[High(Buttons)].Width := Self.Width - Buttons[High(Buttons)].Left + 1;

  Inc(Self.CurrentRowHeight, Buttons[High(Buttons)].Height - 1);
end;

function TIdDTMFPanel.CreateButton(Name: String;
                                   Event: TNotifyEvent): TColourButton;
begin
  Result := TColourButton.Create(nil);
  Result.Caption := Name;
  Result.OnClick := Event;
end;

procedure TIdDTMFPanel.ResizeButtons;
begin
  Self.CurrentRowHeight := 0;
  Self.ButtonHeight := Self.Height div 5 + 1;

  Self.AddRow([Self.Buttons[DTMF1],
               Self.Buttons[DTMF2],
               Self.Buttons[DTMF3],
               Self.Buttons[DTMFA]]);
  Self.AddRow([Self.Buttons[DTMF4],
               Self.Buttons[DTMF5],
               Self.Buttons[DTMF6],
               Self.Buttons[DTMFB]]);
  Self.AddRow([Self.Buttons[DTMF7],
               Self.Buttons[DTMF8],
               Self.Buttons[DTMF9],
               Self.Buttons[DTMFC]]);
  Self.AddRow([Self.Buttons[DTMFStar],
               Self.Buttons[DTMF0],
               Self.Buttons[DTMFHash],
               Self.Buttons[DTMFD]]);
  Self.AddRow([Self.Buttons[DTMFFlash]]);

  // Adjust the last row to take up all remaining vertical space
  if (Self.Buttons[DTMFFlash].Top + Self.Buttons[DTMFFlash].Height < Self.Height) then
    Self.Buttons[DTMFFlash].Height := Self.Height - Self.Buttons[DTMFFlash].Top + 1;

  Self.Constraints.MinHeight := 5*13;
  Self.Constraints.MinWidth  := 4*10;
end;

procedure TIdDTMFPanel.DoOnResize(Sender: TObject);
begin
  Self.ResizeButtons;
end;

procedure TIdDTMFPanel.Flash(Event: TIdRTPTelephoneEventPayload);
var
  Button: TColourButton;
begin
  // We've received a DTMF event. Make the appropriate button flash
  // for the specified amount of time.
  Button := Self.Buttons[Event.Event];
  Button.Color := clBlack xor clWhite;

  // wait Duration milliseconds

  if Event.IsEnd then
    Button.Color := clBlack xor clWhite;
  // what do we do if the ending packet (IsEnd is true after a series of
  // non-end) never arrives? Time out when? 
end;

procedure TIdDTMFPanel.OnNewData(Data: TIdRTPPayload;
                                 Binding: TIdSocketHandle);
begin
  // TODO: Could this indicate the need for a Visitor pattern?
  if Data is TIdRTPTelephoneEventPayload then
    Self.Flash(Data as TIdRTPTelephoneEventPayload);
end;

procedure TIdDTMFPanel.SendDTMF(Event: Byte);
var
  TE: TIdRTPTelephoneEventPayload;
begin
  if Assigned(Self.Processor) then begin
    TE := Processor.Profile.EncodingFor(TelephoneEventEncoding).Clone as TIdRTPTelephoneEventPayload;
    try
      TE.Event     := Event;
      TE.Duration  := 100;
      TE.StartTime := Now;

      Processor.SendData(TE);
    finally
      TE.Free;
    end;
  end;
end;

procedure TIdDTMFPanel.SendDTMF0(Sender: TObject);
begin
  Self.SendDTMF(DTMF0);
end;

procedure TIdDTMFPanel.SendDTMF1(Sender: TObject);
begin
  Self.SendDTMF(DTMF1);
end;

procedure TIdDTMFPanel.SendDTMF2(Sender: TObject);
begin
  Self.SendDTMF(DTMF2);
end;

procedure TIdDTMFPanel.SendDTMF3(Sender: TObject);
begin
  Self.SendDTMF(DTMF3);
end;

procedure TIdDTMFPanel.SendDTMF4(Sender: TObject);
begin
  Self.SendDTMF(DTMF4);
end;

procedure TIdDTMFPanel.SendDTMF5(Sender: TObject);
begin
  Self.SendDTMF(DTMF5);
end;

procedure TIdDTMFPanel.SendDTMF6(Sender: TObject);
begin
  Self.SendDTMF(DTMF6);
end;

procedure TIdDTMFPanel.SendDTMF7(Sender: TObject);
begin
  Self.SendDTMF(DTMF7);
end;

procedure TIdDTMFPanel.SendDTMF8(Sender: TObject);
begin
  Self.SendDTMF(DTMF8);
end;

procedure TIdDTMFPanel.SendDTMF9(Sender: TObject);
begin
  Self.SendDTMF(DTMF9);
end;

procedure TIdDTMFPanel.SendDTMFA(Sender: TObject);
begin
  Self.SendDTMF(DTMFA);
end;

procedure TIdDTMFPanel.SendDTMFB(Sender: TObject);
begin
  Self.SendDTMF(DTMFB);
end;

procedure TIdDTMFPanel.SendDTMFC(Sender: TObject);
begin
  Self.SendDTMF(DTMFC);
end;

procedure TIdDTMFPanel.SendDTMFD(Sender: TObject);
begin
  Self.SendDTMF(DTMFD);
end;

procedure TIdDTMFPanel.SendDTMFFlash(Sender: TObject);
begin
  Self.SendDTMF(DTMFFlash);
end;

procedure TIdDTMFPanel.SendDTMFHash(Sender: TObject);
begin
  Self.SendDTMF(DTMFHash);
end;

procedure TIdDTMFPanel.SendDTMFStar(Sender: TObject);
begin
  Self.SendDTMF(DTMFStar);
end;

procedure TIdDTMFPanel.SetProcessor(Value: TIdSDPPayloadProcessor);
begin
  Self.fProcessor := Value;
  Self.fProcessor.AddDataListener(Self);
end;

end.
