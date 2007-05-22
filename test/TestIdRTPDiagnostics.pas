{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdRTPDiagnostics;

interface

uses
  IdObservable, IdRTP, IdRTPDiagnostics, IdRTPServer, IdTimerQueue,
  TestFramework, TestFrameworkEx;

type
  TestTIdHistogram = class(TTestCase)
  private
    Hist: TIdHistogram;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestClear;
    procedure TestRecordEvent;
  end;

  TestTIdRTPPayloadHistogram = class(TThreadingTestCase,
                                     IIdObserver)
  private
    Hist:     TIdRTPPayloadHistogram;
    Profile:  TIdRTPProfile;
    Receiver: TIdRTPServer;
    Sender:   TIdRTPServer;
    Timer:    TIdTimerQueue;

    procedure OnChanged(Observed: TObject);
    procedure SendData(PayloadType: TIdRTPPayloadType);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetData;
    procedure TestOnePayloadType;
  end;

implementation

uses
  IdSocketHandle, SysUtils, TestFrameworkRtp;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPDiagnostics unit tests');
  Result.AddTest(TestTIdHistogram.Suite);
  Result.AddTest(TestTIdRTPPayloadHistogram.Suite);
end;

//******************************************************************************
//* TestTIdHistogram                                                           *
//******************************************************************************
//* TestTIdHistogram Public methods ********************************************

procedure TestTIdHistogram.SetUp;
begin
  inherited SetUp;

  Self.Hist := TIdHistogram.Create;
end;

procedure TestTIdHistogram.TearDown;
begin
  Self.Hist.Free;

  inherited TearDown;
end;

//* TestTIdHistogram Published methods *****************************************

procedure TestTIdHistogram.TestAssign;
var
  I:       Integer;
  NewHist: TIdHistogram;
begin
  Self.Hist.RecordEvent('foo');
  Self.Hist.RecordEvent('foo');
  Self.Hist.RecordEvent('bar');

  NewHist := TIdHistogram.Create;
  try
    NewHist.Assign(Self.Hist);

    CheckEquals(Self.Hist.Count,
                NewHist.Count,
                'Count differs');

    for I := 0 to Self.Hist.Count - 1 do begin
      CheckEquals(Self.Hist.Entries[I].Name,
                  NewHist.Entries[I].Name,
                  'Name, I = ' + IntToStr(I));
      CheckEquals(Self.Hist.Entries[I].Count,
                  NewHist.Entries[I].Count,
                  'Count, I = ' + IntToStr(I));
    end;

  finally
    NewHist.Free;
  end;
end;

procedure TestTIdHistogram.TestClear;
begin
  Self.Hist.RecordEvent('foo');
  Self.Hist.RecordEvent('foo');
  Self.Hist.RecordEvent('bar');

  Self.Hist.Clear;
  CheckEquals(0, Self.Hist.Count, 'After Clear');
end;

procedure TestTIdHistogram.TestRecordEvent;
begin
  CheckEquals(0, Self.Hist.Count, 'New list');
  Self.Hist.RecordEvent('foo');
  CheckEquals(1, Self.Hist.Count, '"foo" event');
  Self.Hist.RecordEvent('bar');
  CheckEquals(2, Self.Hist.Count, '"bar" event');
  Self.Hist.RecordEvent('foo');
  CheckEquals(2, Self.Hist.Count, 'Another "foo" event');
end;

//******************************************************************************
//* TestTIdRTPPayloadHistogram                                                 *
//******************************************************************************
//* TestTIdRTPPayloadHistogram Public methods **********************************

procedure TestTIdRTPPayloadHistogram.SetUp;
begin
  inherited SetUp;

  Self.Timer   := TIdThreadedTimerQueue.Create(false);
  Self.Profile := TIdAudioVisualProfile.Create;

  Self.Receiver := TIdRTPServer.Create;
  Self.Receiver.Address       := '127.0.0.1';
  Self.Receiver.LocalProfile  := Self.Profile;
  Self.Receiver.RemoteProfile := Self.Profile;
  Self.Receiver.RTPPort       := 58000;
  Self.Receiver.Timer         := Self.Timer;
  Self.Receiver.Active        := true;

  Self.Sender := TIdRTPServer.Create;
  Self.Sender.Address       := '127.0.0.1';
  Self.Sender.LocalProfile  := Self.Profile;
  Self.Sender.RemoteProfile := Self.Profile;
  Self.Sender.RTPPort       := Self.Receiver.RTPPort + 2;
  Self.Sender.Timer         := Self.Timer;
  Self.Sender.Active        := true;

  Self.Hist := TIdRTPPayloadHistogram.Create;
  Self.Receiver.AddListener(Self.Hist);

  Self.Sender.Session.AddReceiver(Self.Receiver.Address,
                                  Self.Receiver.RTPPort)
end;

procedure TestTIdRTPPayloadHistogram.TearDown;
begin
  Self.Hist.Free;
  Self.Sender.Free;
  Self.Receiver.Free;
  Self.Profile.Free;
  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestTIdRTPPayloadHistogram Private methods *********************************

procedure TestTIdRTPPayloadHistogram.OnChanged(Observed: TObject);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdRTPPayloadHistogram.SendData(PayloadType: TIdRTPPayloadType);
var
  Data: TIdRTPPayload;
begin
  Data := Self.Sender.LocalProfile.EncodingFor(PayloadType).Copy;
  try
    Data.StartTime := Now;

    Self.Sender.Session.SendData(Data);
  finally
    Data.Free;
  end;
end;

//* TestTIdRTPPayloadHistogram Published methods *******************************

procedure TestTIdRTPPayloadHistogram.TestGetData;
var
  Hist: TIdHistogram;
begin
  Self.Receiver.NotifyListeners := true;
  Self.Hist.AddObserver(Self);

  Self.ExceptionMessage := 'Waiting for OnChanged: 1';
  Self.SendData(0);
  Self.WaitForSignaled;

  Self.ExceptionMessage := 'Waiting for OnChanged: 2';
  Self.SendData(0);
  Self.WaitForSignaled;

  Self.ExceptionMessage := 'Waiting for OnChanged: 3';
  Self.SendData(13);
  Self.WaitForSignaled;

  Hist := TIdHistogram.Create;
  try
    Self.Hist.GetData(Hist);

    CheckEquals(2, Hist.Count, 'Count');
    CheckEquals(Self.Profile.EncodingFor(0).EncodingName,
                Hist.Entries[0].Name,
                '1st entry name');
    CheckEquals(2,
                Hist.Entries[0].Count,
                '1st entry count');

    CheckEquals(Self.Profile.EncodingFor(13).EncodingName,
                Hist.Entries[1].Name,
                '2nd entry name');
    CheckEquals(1,
                Hist.Entries[1].Count,
                '2nd entry count');
  finally
    Hist.Free;
  end;
end;

procedure TestTIdRTPPayloadHistogram.TestOnePayloadType;
begin
  Self.Receiver.NotifyListeners := true;
  Self.Hist.AddObserver(Self);
  
  CheckEquals(0, Self.Hist.RTPTypeCount, 'No data sent yet');

  Self.SendData(0);

  Self.ExceptionMessage := 'Waiting for OnChanged';
  Self.WaitForSignaled;

  CheckEquals(1, Self.Hist.RTPTypeCount, 'One payload type sent');
end;

initialization
  RegisterTest('RTP Diagnostic tools', Suite);
end.
