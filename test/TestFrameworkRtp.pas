unit TestFrameworkRtp;

interface

uses
  IdRTP, SyncObjs, SysUtils, TestFramework;

type
  TTestRTP = class(TTestCase)
  protected
    ExceptionType:    ExceptClass;
    ExceptionMessage: String;
    ThreadEvent:      TEvent;
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

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

procedure TTestRTP.SetUp;
begin
  inherited SetUp;

  Self.ExceptionType    := Exception;
  Self.ExceptionMessage := 'The event waited for was never fired';
  Self.ThreadEvent      := TEvent.Create(nil, true, false, 'ThreadEvent');
end;

procedure TTestRTP.TearDown;
begin
  ThreadEvent.Free;

  inherited TearDown;
end;

end.
