{
  (c) 2003 Guido Gybels

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Guido Gybels

}
unit audioclasses;

interface

uses MMsystem, Sysutils, Classes, Windows;

const
  ChannelsMono      = 1;
  ChannelsStereo    = 2;
  AnyAudioDevice    = WAVE_MAPPER;
  MaxAudioInstances = 10;
  LockingTimeOut    = 3000;

type
  TAudioFormat = (afPCM, afALaw, afMuLaw);
  TAudioDataSourceType = (stNone, stUnknown, stFile, stMemory, stString, stSocket);

  TAudioData = class;
  TMutex = class;
  TAudioEvent = procedure(Origin: TAudioData) of object;
  TAudioDataEvent = procedure(Origin: TAudioData; BlockCounter: Cardinal) of object;
  TAudioDataVersion = record
    Major: Word;
    Minor: Word;
  end;

  TAudioData = class
  private
    FAudioFormat: TAudioFormat;
    FChannels: Word;
    FSamplesPerSecond: LongWord;
    FBitsPerSample: Word;
    FSource: TStream;
    FAutoFreeSource: Boolean;
    FPlayThread: TThread;
    FFileName: String;
    FOnSourceChange: TAudioEvent;
    FOnStart, FOnStop: TAudioEvent;
    FOnData: TAudioDataEvent;
    FBlockCounter: Cardinal;
    FBusy: Boolean;
    FMutex: TMutex;
    procedure SetAudioFormat(AAudioFormat: TAudioFormat);
    procedure SetChannels(AChannels: Word);
    procedure SetBitsPerSample(AValue: Word);
    procedure SetSampleRate(ARate: Longword);
    function GetBlockSize: Word;
    function GetBytesPerSecond: Longword;
    function GetSourceType: TAudioDataSourceType;
    function GetWaveFormat: tWAVEFORMATEX;
    function GetFormatString: String;
    procedure SetFileName(AFileName: String);
    procedure DataEvent;
    procedure StartEvent;
    procedure StopEvent;
    procedure SetBusy(AValue: Boolean);
    function GetVersion: TAudioDataVersion;
    property WaveFormat: tWAVEFORMATEX read GetWaveFormat;
    property BlockCounter: Cardinal read FBlockCounter;
  protected

  public
    procedure Assign(AStream: TStream);
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: String);
    function Lock: Boolean;
    function UnLock: Boolean;
    function Play(ADevice: Cardinal): Boolean;
    procedure SetFormatParameters(AFormat: TAudioFormat; AChannels: Word; ASamplesPerSecond: LongWord; ABitsPerSample: Word);
    function Stop: Boolean;
    property AudioFormat: TAudioFormat read FAudioFormat write SetAudioFormat;
    property AutoFreeSource: Boolean read FAutoFreeSource write FAutoFreeSource;
    property BitsPerSample: Word read FBitsPerSample write SetBitsPerSample;
    property BlockSize: Word read GetBlockSize;
    property Busy: Boolean read FBusy;
    property BytesPerSecond: LongWord read GetBytesPerSecond;
    property Channels: Word read FChannels write SetChannels;
    property FileName: String read FFileName write SetFileName;
    property FormatString: String read GetFormatString;
    property SamplesPerSecond: LongWord read FSamplesPerSecond write SetSampleRate;
    property Source: TStream read FSource;
    property SourceType: TAudioDataSourceType read GetSourceType;
    property OnData: TAudioDataEvent read FOnData write FOnData;
    property OnSourceChange: TAudioEvent read FOnSourceChange write FOnSourceChange;
    property OnStart: TAudioEvent read FOnStart write FOnStart;
    property OnStop: TAudioEvent read FOnStop write FOnStop;
    property Version: TAudioDataVersion read GetVersion;
  end;

  TMutex = class
  private
    FMutexHandle: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire: Boolean;
    function Release: Boolean;
  end;

  EAudioError = class(Exception);

implementation

uses ScktComp;

const
  OutputBufferSize = 4*1024;
  NumberOfOutputBuffers = 4;

type
  TOutPutBufferState = (bsAvailable, bsBusy);
  TOutPutBuffer = record
    Header: TWaveHdr;
    Buffer: packed array[0..OutputBufferSize-1] of Byte;
    Size: Cardinal;
    State: TOutPutBufferState;
  end;
  TOutPutBuffers = array[1..NumberOfOutputBuffers] of TOutPutBuffer;

  TAudioPlayThread = class(TThread)
  private
    FDeviceID: Cardinal;
    FAudioData: TAudioData;
    StartPosition: Int64;
    AudioPlaybackCriticalSection: TRTLCriticalSection;
    NextFreeBuffer: Integer;
    NextOutPutBuffer: Integer;
    OutPutBuffers: TOutPutBuffers;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TAudioData; ADevice: Cardinal);
    destructor Destroy; override;
    property DeviceID: Cardinal read FDeviceID;
    property AudioData: TAudioData read FAudioData;
  end;

resourcestring
  rsErrorAudioFormatNotSupported   = 'Error: the audio format (%u) is not supported.';
  rsErrorInvalidNumberOfChannels   = 'Error: the number of channels requested (%u) is not supported.';
  rsErrorBitsPerSampleNotSupported = 'Error: the requested bits per sample ratio (%u) is not supported.';
  rsErrorSampleRateNotSupported    = 'Error: the requested sampling rate (%u samples/s) is not supported.';
  rsErrorPlayThreadCreateFailed    = 'Error: the audio play thread could not be created.';
  rsCannotOpenOutputDevice         = 'Error: the audio device (%u) could not be opened (%u).';
  rsErrorWhileUnPreparingHeader    = 'An error (%u) occurred while unpreparing the header %s.';
  rsErrorWhilePreparingHeader      = 'An error (%u) occurred while preparing the header %s.';
  rsPlaybackError                  = 'The audio device %u reported an error %u during playback.';
  rsErrorIllegalStreamType         = 'Error: %s not allowed in this context.';
  rsErrorWhileCreatingMutex        = 'Error while creating a mutex (%u).';
  rsErrorMaxNumberOfInstances      = 'Error: maximum number of AudioClasses instances reached.';

{ Helper functions }

procedure IncreaseCounter(var ACounter: Integer);
begin
  inc(ACounter);
  if ACounter>NumberOfOutputBuffers then ACounter:=1;
end;

procedure WaveCallBack(DeviceHandle: HDRVR; uMsg: UINT; dwUserData: DWORD; dwParam1, dwParam2: DWORD); stdcall;
var
  APlayThread: TAudioPlayThread;
begin
 if uMsg=WOM_DONE then begin
    APlayThread:=TAudioPlayThread(dwUserData);
    EnterCriticalSection(APlayThread.AudioPlaybackCriticalSection);
      APlayThread.OutPutBuffers[APlayThread.NextFreeBuffer].State:=bsAvailable;
    LeaveCriticalSection(APlayThread.AudioPlaybackCriticalSection);
    IncreaseCounter(APlayThread.NextFreeBuffer);
 end;
end;

{ TAudioData }

procedure TAudioData.Assign(AStream: TStream);
begin
  if AStream=nil then raise EAudioError.CreateResFmt(@rsErrorIllegalStreamType,['(nil)']);
  if AStream is TFileStream then raise EAudioError.CreateResFmt(@rsErrorIllegalStreamType,['TFileStream']);
  if AutoFreeSource and Assigned(FSource) then FreeAndNil(FSource);
  FSource:=AStream;
  FFileName:='';
  if Assigned(FOnSourceChange) then FOnSourceChange(Self);
end;

procedure TAudioData.DataEvent;
begin
  inc(FBlockCounter);
  if Assigned(FOnData) then FOnData(Self, BlockCounter);
end;

constructor TAudioData.Create;
begin
  inherited;
  FBusy:=False;
  FSource:=nil;
  FMutex:=TMutex.Create;
  AudioFormat:=afPCM;
  Channels:=ChannelsMono;
  SamplesPerSecond:=8000;
  BitsPerSample:=8;
  AutoFreeSource:=True;
end;

destructor TAudioData.Destroy;
begin
  if AutoFreeSource and Assigned(FSource) then FreeAndNil(FSource);
  FreeAndNil(FMutex);
  inherited;
end;

function TAudioData.GetBlockSize: Word;
begin
  Result:=(Channels*BitsPerSample) div 8;
end;

function TAudioData.GetBytesPerSecond: Longword;
begin
  Result:=SamplesPerSecond * BlockSize;
end;

function TAudioData.GetSourceType: TAudioDataSourceType;
begin
  Result:=stNone;
  if Assigned(FSource) then
  begin
    if FSource is TFileStream then Result:=stFile
       else if FSource is TMemoryStream then Result:=stMemory
            else if FSource is TStringStream then Result:=stString
                 else if FSource is TWinSocketStream then Result:=stSocket
                      else Result:=stUnknown;
  end;
end;

function TAudioData.GetWaveFormat: tWAVEFORMATEX;
begin
  case AudioFormat of
    afPCM: Result.wFormatTag:=1;
    afALaw: Result.wFormatTag:=6;
    afMuLaw: Result.wFormatTag:=7;
  else
    raise EAudioError.CreateResFmt(@rsErrorAudioFormatNotSupported,[Integer(AudioFormat)]);
  end;
  Result.nChannels:=Channels;
  Result.nSamplesPerSec:=SamplesPerSecond;
  Result.nAvgBytesPerSec:=BytesPerSecond;
  Result.nBlockAlign:=BlockSize;
  Result.wBitsPerSample:=BitsPerSample;
  Result.cbSize:=0;
end;

procedure TAudioData.LoadFromFile(AFileName: String);
begin
  if AutoFreeSource and Assigned(FSource) then FreeAndNil(FSource);
  FSource:=TFileStream.Create(AFileName, fmOpenRead);
  FFileName:=AFileName;
  if Assigned(FOnSourceChange) then FOnSourceChange(Self);
end;

function TAudioData.Play(ADevice: Cardinal): Boolean;
var
  APlayThread: TAudioPlayThread; {declare a local variable to prevent typecasting}
begin
  Result:=False;
  if not Busy then
  begin
    SetBusy(True);
    if Assigned(Self.Source) then
    begin
      APlayThread:=TAudioPlayThread.Create(Self, ADevice);
      try
        FBlockCounter:=0;
        FPlayThread:=APlayThread; {Store a pointer to the thread into a local field for use by the stop method}
        APlayThread.Resume;
        Result:=True;
      except
        try
          try
            FreeAndNil(APlayThread);
          finally
            FPlayThread:=nil;
          end;
        except
          raise EAudioError.CreateRes(@rsErrorPlayThreadCreateFailed);
        end;
      end
    end;
  end;
end;

procedure TAudioData.SetAudioFormat(AAudioFormat: TAudioFormat);
begin
  case AAudioFormat of
    afPCM..afMuLaw: FAudioFormat:=AAudioFormat;
  else
    raise EAudioError.CreateResFmt(@rsErrorAudioFormatNotSupported,[Integer(AAudioFormat)]);
  end;
end;

procedure TAudioData.SetBitsPerSample(AValue: Word);
begin
  case AValue of
    8, 16: FBitsPerSample:=AValue;
  else
    raise EAudioError.CreateResFmt(@rsErrorBitsPerSampleNotSupported,[AValue]);
  end;
end;

procedure TAudioData.SetChannels(AChannels: Word);
begin
  case AChannels of
    ChannelsMono, ChannelsStereo: FChannels:=AChannels;
  else
    raise EAudioError.CreateResFmt(@rsErrorInvalidNumberOfChannels,[AChannels]);
  end;
end;

procedure TAudioData.SetFileName(AFileName: String);
begin
  LoadFromFile(AFileName);
end;

procedure TAudioData.SetFormatParameters(AFormat: TAudioFormat;
  AChannels: Word; ASamplesPerSecond: LongWord; ABitsPerSample: Word);
begin
  AudioFormat:=AFormat;
  Channels:=AChannels;
  SamplesPerSecond:=ASamplesPerSecond;
  BitsPerSample:=ABitsPerSample;
end;

procedure TAudioData.SetSampleRate(ARate: Longword);
begin
  case ARate of
    8000, 11025, 22050, 44100: FSamplesPerSecond:=ARate;
  else
    raise EAudioError.CreateResFmt(@rsErrorSampleRateNotSupported,[ARate]);
  end;
end;

function TAudioData.Stop: Boolean;
begin
  Result:=True;
  try
    if Assigned(FPlayThread) then FPlayThread.Terminate;
    FPlayThread:=nil;
  except
    Result:=False;
  end;
end;

procedure TAudioData.StartEvent;
begin
  if Assigned(FOnStart) then FOnStart(Self);
end;

procedure TAudioData.StopEvent;
begin
  SetBusy(False);
  if Assigned(FOnStop) then FOnStop(Self);
end;

function TAudioData.GetFormatString: String;
begin
  case AudioFormat of
    afPCM: Result:='PCM, ';
    afALaw: Result:='aLaw, ';
    afMuLaw: Result:='muLaw, ';
  end;
  case Channels of
    1: Result:=Result+'Mono, ';
    2: Result:=Result+'Stereo, ';
  end;
  Result:=Result+IntToStr(BitsPerSample)+'-bit, '
         +IntToStr(SamplesPerSecond)+'/s';
end;

procedure TAudioData.SetBusy(AValue: Boolean);
begin
  FBusy:=AValue;
end;

function TAudioData.GetVersion: TAudioDataVersion;
begin
  Result.Major:=0;
  Result.Minor:=6;
end;

function TAudioData.Lock: Boolean;
begin
  Result:=FMutex.Acquire;
end;

function TAudioData.UnLock: Boolean;
begin
  Result:=FMutex.Release;
end;

{ TAudioPlayThread }

constructor TAudioPlayThread.Create(Owner: TAudioData; ADevice: Cardinal);
var
  I: Integer;
begin
  inherited Create(True);
  {Set up the Critical Section}
  InitializeCriticalSection(AudioPlaybackCriticalSection);
  {Set default values}
  FreeOnTerminate:=True;
  NextOutPutBuffer:=1;
  NextFreeBuffer:=1;
  StartPosition:=0;
  {Initialise all the buffers}
  for I:=1 to NumberOfOutputBuffers do
  begin
    FillChar(OutPutBuffers[I].Buffer,OutputBufferSize,0);
    OutPutBuffers[I].Size:=0;
    OutPutBuffers[I].State:=bsAvailable;
    OutPutBuffers[I].Header.dwFlags:=0;
    OutPutBuffers[I].Header.lpData:=@OutPutBuffers[I].Buffer;
    OutPutBuffers[I].Header.dwBytesRecorded:=0;
    OutPutBuffers[I].Header.dwUser:=0;
    OutPutBuffers[I].Header.dwLoops:=1;
  end;
  FAudioData:=Owner;
  FDeviceID:=ADevice;
end;

destructor TAudioPlayThread.Destroy;
begin
  DeleteCriticalSection(AudioPlaybackCriticalSection);
  inherited;
end;

procedure TAudioPlayThread.Execute;
var
  BytesRead: Cardinal;
  WaveFormat: tWAVEFORMATEX;
  WaveDeviceHandle: HWAVEOUT;
  I, Error: Integer;
  Counter: Integer;
  CurrentPosition: Int64;
begin
  inherited;
  if AudioData.SourceType<>stNone then
  begin
    if AudioData.SourceType=stFile
       then AudioData.Source.Seek(0,soFromBeginning);
    WaveFormat:=AudioData.WaveFormat;
    Error:=waveOutOpen(@WaveDeviceHandle,DeviceID,@WaveFormat,Cardinal(@WaveCallBack),Cardinal(Self),CALLBACK_FUNCTION);
    try
      if Error<>0 then
         raise EAudioError.CreateResFmt(@rsCannotOpenOutputDevice,[DeviceID,Error]);
      Synchronize(AudioData.StartEvent);
      repeat
        Sleep(100);
        while OutPutBuffers[NextOutPutBuffer].State<>bsAvailable do
        begin
          Sleep(100);
        end;
        EnterCriticalSection(AudioPlaybackCriticalSection);
          OutPutBuffers[NextOutPutBuffer].State:=bsBusy;
        LeaveCriticalSection(AudioPlaybackCriticalSection);
        {Unprepare this buffer if it was in use}
        if (OutPutBuffers[NextOutPutBuffer].Header.dwFlags and WHDR_PREPARED)<>0 then
        begin
          Error:=waveOutUnprepareHeader(WaveDeviceHandle,@OutPutBuffers[NextOutPutBuffer].Header,SizeOf(TWaveHdr));
          if Error<>0
             then raise EAudioError.CreateResFmt(@rsErrorWhileUnPreparingHeader,[Error,'for playback']);
          OutPutBuffers[NextOutPutBuffer].Header.dwFlags:=0;
        end;
        if AudioData.Lock then
        begin
          try
            CurrentPosition:=AudioData.Source.Position;
            if AudioData.SourceType<>stFile
               then AudioData.Source.Seek(StartPosition,soFromBeginning);
            BytesRead:=AudioData.Source.Read(OutPutBuffers[NextOutPutBuffer].Buffer,OutPutBufferSize);
            if AudioData.SourceType<>stFile then
            begin
              StartPosition:=AudioData.Source.Position;
              AudioData.Source.Position:=CurrentPosition;
            end;
          finally
            AudioData.UnLock;
          end;
        end
        else BytesRead:=0;
        if BytesRead>0 then {only if there actually IS data to be played...}
        begin
          OutPutBuffers[NextOutPutBuffer].Header.dwBufferLength:=BytesRead;
          OutPutBuffers[NextOutPutBuffer].Header.dwFlags:=WHDR_INQUEUE;
          Error:=waveOutPrepareHeader(WaveDeviceHandle,@OutPutBuffers[NextOutPutBuffer].Header,SizeOf(TWaveHdr));
          if Error<>0 then
             raise EAudioError.CreateResFmt(@rsErrorWhilePreparingHeader,[Error,'for playback']);
          Error:=waveOutWrite(WaveDeviceHandle,@OutPutBuffers[NextOutPutBuffer].Header,SizeOf(TWaveHdr));
          if Error<>0 then
             raise EAudioError.CreateResFmt(@rsPlaybackError,[DeviceID,Error]);
          Synchronize(AudioData.DataEvent);
          IncreaseCounter(NextOutPutBuffer);
        end
        else
        begin {If no data read, than no need to queue buffer, have to reset buffer flag to available and...}
          EnterCriticalSection(AudioPlaybackCriticalSection);
            OutPutBuffers[NextOutPutBuffer].State:=bsAvailable;
          LeaveCriticalSection(AudioPlaybackCriticalSection);
          {...bail out if end of file is reached in case of a filestream}
          if (AudioData.SourceType=stFile)
             then Break;
        end;
      until Terminated;
    finally
      if WaveDeviceHandle<>0 then
      begin
        try
          {Unprepare any remaining buffers}
          for I:=1 to NumberOfOutputBuffers do
          begin
            Counter:=1;
            {Wait for every non-free buffer to become available or time out}
            while OutPutBuffers[I].State<>bsAvailable do
            begin
              Sleep(100);
              Inc(Counter);
              if Counter>50
                 then break;
            end;
            {Unprepare the buffer if necessary}
            if (OutPutBuffers[I].Header.dwFlags and WHDR_PREPARED)<>0 then
            begin
              Error:=waveOutUnprepareHeader(WaveDeviceHandle,@OutPutBuffers[I].Header,SizeOf(TWaveHdr));
              if Error<>0 then
                 raise EAudioError.CreateResFmt(@rsErrorWhileUnPreparingHeader,[Error,'while finalising']);
              OutPutBuffers[I].Header.dwFlags:=0;
            end;
          end;
        finally
          try
            waveOutReset(WaveDeviceHandle);
          finally
            waveOutClose(WaveDeviceHandle);
          end;
        end;
      end;
    end;
  end;
  Synchronize(AudioData.StopEvent);
end;

{ TMutex }

function TMutex.Acquire: Boolean;
begin
  Result:=WaitForSingleObject(FMutexHandle,LockingTimeOut)=WAIT_OBJECT_0;
end;

constructor TMutex.Create;
const
  MutexNameBase = 'AUDIOCLASSESMUTEX';
var
  Error: Integer;
  Instance: Integer;
  MutexName: String;
begin
  inherited Create;
  Instance:=1;
  repeat
    MutexName:=MutexNameBase+IntToStr(Instance);
    FMutexHandle:=CreateMutex(nil,False,PChar(MutexName));
    if FMutexHandle=0 then
    begin
      Error:=GetLastError;
      if Error<>ERROR_ALREADY_EXISTS then raise EAudioError.CreateResFmt(@rsErrorWhileCreatingMutex,[Error]);
    end
    else Break;
    inc(Instance);
    if Instance>MaxAudioInstances then raise EAudioError.CreateRes(@rsErrorMaxNumberOfInstances);
  until FMutexHandle<>0
end;

destructor TMutex.Destroy;
begin
  CloseHandle(FMutexHandle);
  inherited;
end;

function TMutex.Release: Boolean;
begin
  Result:=ReleaseMutex(FMutexHandle);
end;

end.
