unit IdRTPBase;

interface

uses
  Classes, IdSdpParser, SysUtils;

type
  TIdCardinalArray  = array of Cardinal;
  TIdNTPTimestamp   = Cardinal;
  TIdRTPCsrcCount   = 0..15;
  TIdRTPPayloadType = 0..127;
  TIdRTPSequenceNo  = Word;
  TIdRTPVersion     = 0..3;
  TIdT140BlockCount = Word;

  // I am an Encoding. I am described in an SDP payload (RFC 2327),
  // and instantiated by things that need to describe these sorts
  // of encodings. I am a Value Object.
  TIdRTPEncoding = class(TObject)
  private
    fClockRate:  Cardinal;
    fName:       String;
    fParameters: String;
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; virtual;
    constructor Create(const Src: TIdRTPEncoding); overload; virtual;

    function AsString: String; virtual;
    function Clone: TIdRTPEncoding; virtual;
    function IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
    function IsNull: Boolean; virtual;
    function IsReserved: Boolean; virtual;

    property ClockRate:  Cardinal read fClockRate;
    property Name:       String   read fName;
    property Parameters: String   read fParameters;
  end;

  TIdRTPT140Encoding = class(TIdRTPEncoding);

  // I am the Null Encoding. I represent the fact that there is no defined
  // encoding.
  TIdRTPNullEncoding = class(TIdRTPEncoding)
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; override;
    constructor Create(const Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function Clone: TIdRTPEncoding; override;
    function IsNull: Boolean; override;
  end;

  // I am a Reserved or Unassigned encoding in an RTP profile. In other words, I
  // do nothing other than say to you "you may not use this payload type".
  TIdRTPReservedEncoding = class(TIdRTPEncoding)
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; override;
    constructor Create(const Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function Clone: TIdRTPEncoding; override;
    function IsReserved: Boolean; override;
  end;

  TIdPayloadArray = array[Low(TIdRTPPayloadType)..High(TIdRTPPayloadType)] of TIdRTPEncoding;

  // I am the payload in an RTP packet. My subclasses implement appropriate
  // parsing and outputting.
  //
  // I offer a Flyweight Null Payload.
  TIdRTPPayload = class(TObject)
  public
    class function CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
    class function CreateFrom(const Encoding: TIdRTPEncoding;
                              Src: TStream): TIdRTPPayload;
    class function NullPayload: TIdRTPPayload;

    function  IsNull: Boolean; virtual;
    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;
  end;

  // I am the Null payload - a Null Object representing the absence of a
  // payload.
  TIdNullPayload = class(TIdRTPPayload)
  public
    function IsNull: Boolean; override;
  end;

  // I am a T.140 payload, as defined in RFC 2793 (and the bis draft)
  TIdT140Payload = class(TIdRTPPayload)
  private
    fBlock:      String;
    fBlockCount: TIdT140BlockCount;
  public
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Block:      String            read fBlock write fBlock; //TODO: this must be UTF-8
    property BlockCount: TIdT140BlockCount read fBlockCount write fBlockCount;
  end;

  // I am the Raw payload. That means that my content is completely unparsed.
  // I'm mostly a fallback case for a malconfigured RTP server.
  TIdRawPayload = class(TIdRTPPayload)
  private
    fData: String;
  public
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Data: String read fData write fData;
  end;

  ENoPayloadTypeFound = class(Exception);

function EncodeAsString(Value: Cardinal): String; overload;
function EncodeAsString(Value: Word): String; overload;
function HtoNL(Value: Cardinal): Cardinal;
function HtoNS(Value: Word): Word;
function NtoHL(Value: Cardinal): Cardinal;
function NtoHS(Value: Word): Cardinal;

function  ReadCardinal(Src: TStream): Cardinal;
function  ReadRemainderOfStream(Src: TStream): String;
function  ReadWord(Src: TStream): Word;
procedure WriteCardinal(Dest: TStream; Value: Cardinal);
procedure WriteWord(Dest: TStream; Value: Word);

const
  InterleavedT140ClockRate    = 8000;
  RedundancyEncodingParameter = 'red';
  T140ClockRate               = 1000;
  T140EncodingName            = 't140';

  InterleavedT140MimeType = 'audio/t140';
  RedundantT140MimeType   = 'text/RED';
  T140MimeType            = 'text/t140';

implementation

uses
  IdSipConsts;

var
  GNullPayload: TIdRTPPayload;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function EncodeAsString(Value: Cardinal): String;
begin
  Result := Chr((Value and $FF000000) shr 24)
          + Chr((Value and $00FF0000) shr 16)
          + Chr((Value and $0000FF00) shr 8)
          + Chr( Value and $000000FF);
end;

function EncodeAsString(Value: Word): String;
begin
  Result := Chr((Value and $FF00) shr 8)
          + Chr( Value and $00FF);
end;

function HtoNL(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $FF000000) shr 24)
         or ((Value and $00FF0000) shr 8)
         or ((Value and $0000FF00) shl 8)
         or ((Value and $000000FF) shl 24);
end;

function HtoNS(Value: Word): Word;
begin
  Result := ((Value and $00FF) shl 8) or ((Value  and $FF00) shr 8);
end;

function NtoHL(Value: Cardinal): Cardinal;
begin
  Result := HtoNL(Value);
end;

function NtoHS(Value: Word): Cardinal;
begin
  Result := HtoNS(Value);
end;

function ReadCardinal(Src: TStream): Cardinal;
begin
  Src.Read(Result, SizeOf(Result));
  Result := NtoHL(Result);
end;

function ReadRemainderOfStream(Src: TStream): String;
const
  BufLen = 100;
var
  Buf:  array[1..BufLen] of Char;
  Read: Integer;
begin
  Result := '';

  repeat
    Read := Src.Read(Buf, BufLen);
    Result := Result + Copy(Buf, 1, Read);
  until (Read < BufLen);
end;

function ReadWord(Src: TStream): Word;
begin
  Src.Read(Result, SizeOf(Result));
  Result := NtoHS(Result);
end;

procedure WriteCardinal(Dest: TStream; Value: Cardinal);
begin
  Value := HtoNL(Value);
  Dest.Write(Value, SizeOf(Value));
end;

procedure WriteWord(Dest: TStream; Value: Word);
begin
  Value := HtoNS(Value);
  Dest.Write(Value, SizeOf(Value));
end;

//******************************************************************************
//* TIdRTPEncoding                                                             *
//******************************************************************************
//* TIdRTPEncoding Public methods **********************************************

constructor TIdRTPEncoding.Create(const Name: String;
                                  const ClockRate: Cardinal;
                                  const Parameters: String = '');
begin
  inherited Create;

  fClockRate  := ClockRate;
  fName       := Name;
  fParameters := Parameters;
end;

constructor TIdRTPEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create;

  fClockRate  := Src.ClockRate;
  fName       := Src.Name;
  fParameters := Src.Parameters;
end;

function TIdRTPEncoding.AsString: String;
begin
  Result := Self.Name + '/' + IntToStr(Self.ClockRate);

  if (Self.Parameters <> '') then
    Result := Result + '/' + Self.Parameters;
end;

function TIdRTPEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPEncoding.Create(Self);
end;

function TIdRTPEncoding.IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
begin
  Result := (Self.Name = OtherEncoding.Name)
        and (Self.ClockRate = OtherEncoding.ClockRate)
        and (Self.Parameters = OtherEncoding.Parameters);
end;

function TIdRTPEncoding.IsNull: Boolean;
begin
  Result := false;
end;

function TIdRTPEncoding.IsReserved: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdRTPNullEncoding                                                         *
//******************************************************************************
//* TIdRTPNullEncoding Public methods ******************************************

constructor TIdRTPNullEncoding.Create(const Name: String;
                                      const ClockRate: Cardinal;
                                      const Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPNullEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create('', 0, '');
end;

function TIdRTPNullEncoding.AsString: String;
begin
  Result := '';
end;

function TIdRTPNullEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPNullEncoding.Create(Self);
end;

function TIdRTPNullEncoding.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRTPReservedEncoding                                                     *
//******************************************************************************
//* TIdRTPReservedEncoding Public methods **************************************

constructor TIdRTPReservedEncoding.Create(const Name: String;
                                          const ClockRate: Cardinal;
                                          const Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPReservedEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create('', 0, '');
end;

function TIdRTPReservedEncoding.AsString: String;
begin
  Result := '';
end;

function TIdRTPReservedEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPReservedEncoding.Create(Self);
end;

function TIdRTPReservedEncoding.IsReserved: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRTPPayload                                                              *
//******************************************************************************
//* TIdRTPPayload Public methods ***********************************************

class function TIdRTPPayload.CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
begin
  if Encoding.IsNull then
    Result := TIdRawPayload.Create
  else if (Encoding.Name = T140EncodingName) then
    Result := TIdT140Payload.Create
  else
    Result := TIdRawPayload.Create;
end;

class function TIdRTPPayload.CreateFrom(const Encoding: TIdRTPEncoding;
                                        Src: TStream): TIdRTPPayload;
begin
  Result := Self.CreatePayload(Encoding);
  try
    Result.ReadFrom(Src);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdRTPPayload.NullPayload: TIdRTPPayload;
begin
  if not Assigned(GNullPayload) then
    GNullPayload := TIdNullPayload.Create;

  Result := GNullPayload;
end;

function TIdRTPPayload.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdRTPPayload.ReadFrom(Src: TStream);
begin
end;

procedure TIdRTPPayload.PrintOn(Dest: TStream);
begin
end;

//******************************************************************************
//* TIdNullPayload                                                             *
//******************************************************************************
//* TIdNullPayload Public methods **********************************************

function TIdNullPayload.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdT140Payload                                                             *
//******************************************************************************
//* TIdT140Payload Public methods **********************************************

procedure TIdT140Payload.ReadFrom(Src: TStream);
begin
  Self.BlockCount := ReadWord(Src);

  Self.Block := ReadRemainderOfStream(Src);
end;

procedure TIdT140Payload.PrintOn(Dest: TStream);
begin
  WriteWord(Dest, Self.BlockCount);

  if (Self.Block <> '') then
    Dest.Write(PChar(Self.Block)^, Length(Self.Block));
end;

//******************************************************************************
//* TIdRawPayload                                                              *
//******************************************************************************
//* TIdRawPayload Public methods ***********************************************

procedure TIdRawPayload.ReadFrom(Src: TStream);
begin
  Self.Data := ReadRemainderOfStream(Src);
end;

procedure TIdRawPayload.PrintOn(Dest: TStream);
begin
  Dest.Write(PChar(Self.Data)^, Length(Self.Data));
end;

initialization
finalization
  GNullPayload.Free;
end.
