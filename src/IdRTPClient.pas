unit IdRTPClient;

interface

uses
  IdUDPClient, IdRTPServer;

type
  TIdRTPClient = class(TIdUDPClient)
  public
    procedure Send(APacket: TIdRTPPacket); overload;
  end;

implementation

uses
  Classes;

//******************************************************************************
//* TIdRTPClient                                                               *
//******************************************************************************
//* TIdRTPClient Public methods ************************************************

procedure TIdRTPClient.Send(APacket: TIdRTPPacket);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    APacket.PrintOn(S);
    Self.Send(S.DataString);
  finally
    S.Free;
  end;
end;

end.
