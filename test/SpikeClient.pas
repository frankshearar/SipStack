unit SpikeClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Invite: TButton;
    Log: TMemo;
    procedure InviteClick(Sender: TObject);
  private

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  IdSipConsts, IdSipCore, IdSipHeaders, IdSipMessage, IdTcpClient, IdURI;

procedure TForm1.InviteClick(Sender: TObject);
var
  Client:   TIdTcpClient;
  Line:     String;
  Request:  TIdSipRequest;
  ToHeader: TIdSipToHeader;
  UA:       TIdSipUserAgentCore;
begin
  UA := TIdSipUserAgentCore.Create;
  try
    UA.From.Address.URI    := 'sip:franks@127.0.0.1';
    UA.From.Tag            := '1';
    UA.Contact.Address.URI := 'sip:franks@127.0.0.1';
    UA.HostName := '127.0.0.1';

    ToHeader := TIdSipToHeader.Create;
    try
      ToHeader.Value := 'sip:franks@127.0.0.1';

      Request := UA.CreateInvite(ToHeader);
      try
        Request.CSeq.Value := '314159 INVITE';
        Request.MaxForwards := 70;

        Client := TIdTcpClient.Create(nil);
        try
          Client.Host := 'wsfrank';
          Client.Port := IdPORT_SIP;
          Client.Connect;
          try
            Client.Write(Request.AsString);

            Line := Client.ReadLn(#$A, 1000);
            while (Line <> '') do begin
              Self.Log.Lines.Add(Line);
              Line := Client.ReadLn(#$A, 1000);
            end;
          finally
            Client.Disconnect;
          end;
        finally
          Client.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      ToHeader.Free;
    end;
  finally
    UA.Free;
  end;
end;

end.
 