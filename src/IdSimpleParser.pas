unit IdSimpleParser;

interface

uses
  Classes, SysUtils;

type
  TIdSimpleParser = class(TObject)
  private
    fCurrentLine: Cardinal;
    fSource:      TStream;
    procedure IncCurrentLine;
  protected
    procedure ResetCurrentLine;
  public
    class function IsAlphaNumeric(const Token: String): Boolean;
    class function IsDigit(const C: Char): Boolean;
    class function IsNumber(const Number: String): Boolean;
    constructor Create; virtual;

    function  CurrentLine: Cardinal;
    function  Eof: Boolean;
    function  Peek: Char;
    function  PeekLine: String;
    function  ReadOctet: Char;
    function  ReadOctets(Count: Cardinal): String;
    function  ReadLn: String;
    function  ReadFirstNonBlankLine: String;

    property  Source: TStream read fSource write fSource;
  end;

  EParser = class(Exception);

const
  BadSyntax        = 'Bad syntax';
  DamagedLineEnd   = 'Damaged line end at line %d, expected %s but was %s';
  EmptyInputStream = 'Empty input stream';
  MalformedToken   = 'Malformed %s: ''%s''';

const
  Alphabet = ['a'..'z', 'A'..'Z'];
  Digits   = ['0'..'9'];

implementation

//******************************************************************************
//* TIdSimpleParser                                                            *
//******************************************************************************
//* TIdSimpleParser Public methods *********************************************

class function TIdSimpleParser.IsAlphaNumeric(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if (Result) then
    for I := 1 to Length(Token) do begin
      Result := Result and (Self.IsDigit(Token[I])
                        or (Token[I] in Alphabet));
      if not Result then Break;
    end;
end;

class function TIdSimpleParser.IsDigit(const C: Char): Boolean;
begin
  Result := C in Digits;
end;

class function TIdSimpleParser.IsNumber(const Number: String): Boolean;
var
  I: Integer;
begin
  Result := Number <> '';

  if (Result) then
    for I := 1 to Length(Number) do begin
      Result := Result and Self.IsDigit(Number[I]);

      if not Result then Break;
    end;
end;

constructor TIdSimpleParser.Create;
begin
  inherited Create;

  Self.ResetCurrentLine;
end;

function TIdSimpleParser.CurrentLine: Cardinal;
begin
  Result := fCurrentLine;
end;

function TIdSimpleParser.Eof: Boolean;
var
  C: Char;
  N: Integer;
begin
  N := Self.Source.Read(C, 1);
  Result := (N = 0);

  if not Result then
    Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSimpleParser.Peek: Char;
begin
  Result := Self.ReadOctet;
  Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSimpleParser.PeekLine: String;
var
  C: Char;
begin
  Result := '';
  while not Self.Eof and not (Self.Peek = #13) do begin
    C := Self.ReadOctet;
    Result := Result + C;
  end;

  Self.Source.Seek(-Length(Result), soFromCurrent);
end;

function TIdSimpleParser.ReadOctet: Char;
begin
  Self.Source.Read(Result, 1);
end;

function TIdSimpleParser.ReadOctets(Count: Cardinal): String;
begin
  Result := '';
  while not Self.Eof and (Count > 0) do begin
    Result := Result + Self.ReadOctet;
    Dec(Count);
  end;
end;

function TIdSimpleParser.ReadLn: String;
var
  C: Char;
begin
  Result := Self.PeekLine;
  Self.Source.Seek(Length(Result), soFromCurrent);

  if (not Self.Eof) then begin
    C := Self.ReadOctet;
    Assert(C = #13, Format(DamagedLineEnd, [Self.CurrentLine, '#$0D', '#$' + IntToHex(Ord(C), 2)]));
    C := Self.ReadOctet;
    Assert(C = #10, Format(DamagedLineEnd, [Self.CurrentLine, '#$0A', '#$' + IntToHex(Ord(C), 2)]));
  end;

  Self.IncCurrentLine;
end;

function TIdSimpleParser.ReadFirstNonBlankLine: String;
begin
  Result := Self.ReadLn;
  while (Result = '') and not Self.Eof do
    Result := Self.ReadLn;
end;

//* TIdSimpleParser Protected methods ******************************************

procedure TIdSimpleParser.ResetCurrentLine;
begin
  fCurrentLine := 1;
end;

//* TIdSimpleParser Private methods ********************************************

procedure TIdSimpleParser.IncCurrentLine;
begin
  Inc(fCurrentLine);
end;

end.
