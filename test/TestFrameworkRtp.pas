unit TestFrameworkRtp;

interface

uses
  IdRTP, TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TTestRTP                                                                   *
//******************************************************************************
//* TTestRTP Public methods ****************************************************

procedure TTestRTP.CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
var
  I: Integer;
begin
  CheckEquals(Expected.Version,            Received.Version,              'Version');
  CheckEquals(Expected.HasPadding,         Received.HasPadding,           'HasPadding');
  CheckEquals(Expected.CsrcCount,          Received.CsrcCount,            'CSRC count');
  CheckEquals(Expected.IsMarker,           Received.IsMarker,             'IsMarker');
  CheckEquals(Expected.PayloadType,        Received.PayloadType,          'PayloadType');
  CheckEquals(Expected.SequenceNo,         Received.SequenceNo,           'SequenceNo');
  CheckEquals(Integer(Expected.Timestamp), Integer(Received.Timestamp),   'Timestamp');
  CheckEquals(Expected.SyncSrcID,          Received.SyncSrcID,            'SSRC ID');

  for I := 0 to Expected.CsrcCount - 1 do
    CheckEquals(Integer(Expected.CsrcIDs[I]),
                Integer(Received.CsrcIDs[I]),
                IntToStr(I) + 'th CSRC ID');
end;

end.
