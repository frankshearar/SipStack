{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdUnicode;

interface

uses
  SysUtils, Types;

const
  LineSeparator                 = $2028;
  LineSeparatorChar             = WideChar(LineSeparator);
  Utf8ZeroWidthNonBreakingSpace = #$EF#$BB#$BF;
  ZeroWidthNonBreakingSpace     = $FEFF;
  ZeroWidthNonBreakingSpaceChar = WideChar(ZeroWidthNonBreakingSpace);

const
  UnicodeHighSurrogateStart = $D800;
  UnicodeHighSurrogateEnd   = $DBFF;
  UnicodeLowSurrogateStart  = $DC00;
  UnicodeLowSurrogateEnd    = $DFFF;
  UTF8BOM                   = Utf8ZeroWidthNonBreakingSpace;

// Unicode functions
function CodePointToUTF8(CodePoint: DWord): String;
function HighSurrogate(CodePoint: DWord): Word;
function IsHighSurrogate(W: Word): Boolean;
function IsLowSurrogate(W: Word): Boolean;
function LowSurrogate(CodePoint: DWord): Word;
function SurrogateToCodePoint(HighSurrogate, LowSurrogate: Word): DWord;
function UTF16LEToUTF8(const W: WideString): String;
function UTF8ToUTF16LE(const S: String): WideString;

function LastPosW(const Needle, Haystack: WideString; Start: Integer = -1): Integer;
function PosW(const Needle, Haystack: WideString): Integer;
function StringReplaceW(const S, OldPattern, NewPattern: WideString; ReplaceAll: Boolean = false): WideString;

implementation

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function CodePointToUTF8(CodePoint: DWord): String;
begin
  if      (CodePoint < $00000080) then begin
    // 0000 0000-0000 007F
    Result := Chr(CodePoint)
  end
  else if (CodePoint < $00000800) then begin
    // 0000 0080-0000 07FF
    Result := Chr($C0 or ((CodePoint shr 6) and $FF))
            + Chr($80 or  (CodePoint and $3F));
  end
  else if (CodePoint < $00010000) then begin
    // 0000 0800-0000 FFFF
    Result := Chr($E0 or ((CodePoint shr 12) and $FF))
            + Chr($80 or ((CodePoint shr 6)  and $3F))
            + Chr($80 or  (CodePoint and $3F));
  end
  else if (CodePoint < $00200000) then begin
    // 0001 0000-001F FFFF
    Result := Chr($F0 or ((CodePoint shr 18) and $FF))
            + Chr($80 or ((CodePoint shr 12) and $3F))
            + Chr($80 or ((CodePoint shr 6)  and $3F))
            + Chr($80 or  (CodePoint and $3F));
  end
  else if (CodePoint < $04000000) then begin
    // 0020 0000-03FF FFFF
    Result := Chr($F8 or ((CodePoint shr 24) and $FF))
            + Chr($80 or ((CodePoint shr 18) and $3F))
            + Chr($80 or ((CodePoint shr 12) and $3F))
            + Chr($80 or ((CodePoint shr 6)  and $3F))
            + Chr($80 or  (CodePoint and $3F));
  end
  else if (CodePoint < $80000000) then begin
    // 0400 0000-7FFF FFFF
    Result := Chr($FC or ((CodePoint shr 30) and $FF))
            + Chr($80 or ((CodePoint shr 24) and $3F))
            + Chr($80 or ((CodePoint shr 18) and $3F))
            + Chr($80 or ((CodePoint shr 12) and $3F))
            + Chr($80 or ((CodePoint shr 6)  and $3F))
            + Chr($80 or  (CodePoint and $3F));
  end
  else
    raise EConvertError.Create('Not a Unicode character');
end;

function HighSurrogate(CodePoint: DWord): Word;
var
  U:    DWord;
  X, W: Word;
begin
  X := CodePoint and $FFFF;
  U := (CodePoint shr 16) and ((1 shl 5) - 1);
  W := U - 1;

  Result := UnicodeHighSurrogateStart or (W shl 6) or (X shr 10);
end;

function IsHighSurrogate(W: Word): Boolean;
begin
  Result := (UnicodeHighSurrogateStart <= W)
        and (W <= UnicodeHighSurrogateEnd);
end;

function IsLowSurrogate(W: Word): Boolean;
begin
  Result := (UnicodeLowSurrogateStart <= W)
        and (W <= UnicodeLowSurrogateEnd);
end;

function LowSurrogate(CodePoint: DWord): Word;
var
  X: Word;
begin
  X := CodePoint and $FFFF;

  Result := UnicodeLowSurrogateStart or (X and ((1 shl 10) - 1));
end;

function SurrogateToCodePoint(HighSurrogate, LowSurrogate: Word): DWord;
const
  SURROGATE_OFFSET = $10000
                   - (UnicodeHighSurrogateStart shl 10)
                   - UnicodeLowSurrogateStart;
begin
  Result := (HighSurrogate shl 10) + LowSurrogate + SURROGATE_OFFSET;
end;

function UTF16LEToUTF8(const W: WideString): String;
var
  CodePoint: DWord;
  I:         Integer;
begin
  Result := '';
  I := 1;
  while (I <= Length(W)) do begin
    if not IsHighSurrogate(Ord(W[I])) then begin
      CodePoint := Ord(W[I]);
    end
    else begin
      if (Length(W) < I + 1) then
        raise EConvertError.Create('String too short for last UTF-16 char');
      CodePoint := SurrogateToCodePoint(Ord(W[I]), Ord(W[I + 1]));

      Inc(I);
    end;

    Result := Result + CodePointToUTF8(CodePoint);
    Inc(I);
  end;
end;

function  UTF8ToUTF16LE(const S: String): WideString;
var
  CodePoint: DWord;
  I:         Integer;
begin
  CodePoint := 0;
  Result    := '';
  I         := 1;
  while (I <= Length(S)) do begin
    if (Ord(S[I]) < $80) then
      // 0000 0000-0000 007F
      // 0xxxxxxx
      CodePoint := Ord(S[I])
    else if (Ord(S[I]) < $E0) then begin
      // 0000 0080-0000 07FF
      // 110xxxxx 10xxxxxx
      if (Length(S) < I + 1) then
        raise EConvertError.Create('String too short for last UTF-8 char');
      CodePoint := ((Ord(S[I])  and $1F) shl 6)
               + (Ord(S[I + 1]) and $3F);
      Inc(I);
    end
    else if (Ord(S[I]) < $F0) then begin
      // 0000 0800-0000 FFFF
      // 1110xxxx 10xxxxxx 10xxxxxx
      if (Length(S) < I + 2) then
        raise EConvertError.Create('String too short for last UTF-8 char');
      CodePoint := ((Ord(S[I])   and $0F) shl 12)
               + ((Ord(S[I + 1]) and $3F) shl 6)
               +  (Ord(S[I + 2]) and $3F);
      Inc(I, 2);
    end
    else if (Ord(S[I]) < $F8) then begin
      // 0001 0000-001F FFFF
      // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      if (Length(S) < I + 3) then
        raise EConvertError.Create('String too short for last UTF-8 char');
      CodePoint := ((Ord(S[I])   and $07) shl 18)
               + ((Ord(S[I + 1]) and $3F) shl 12)
               + ((Ord(S[I + 2]) and $3F) shl 6)
               +  (Ord(S[I + 3]) and $3F);
      Inc(I, 3);
    end
    else if (Ord(S[I]) < $FC) then begin
      // 0020 0000-03FF FFFF
      // 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
      if (Length(S) < I + 4) then
        raise EConvertError.Create('String too short for last UTF-8 char');
      CodePoint := ((Ord(S[I])   and $03) shl 24)
               + ((Ord(S[I + 1]) and $3F) shl 18)
               + ((Ord(S[I + 2]) and $3F) shl 12)
               + ((Ord(S[I + 3]) and $3F) shl 6)
               +  (Ord(S[I + 4]) and $3F);
      Inc(I, 4);
    end
    else if (Ord(S[I]) < $FE) then begin
      // 0400 0000-7FFF FFFF
      // 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
      if (Length(S) < I + 5) then
        raise EConvertError.Create('String too short for last UTF-8 char');
      CodePoint := ((Ord(S[I])   and $01) shl 30)
               + ((Ord(S[I + 1]) and $3F) shl 24)
               + ((Ord(S[I + 2]) and $3F) shl 18)
               + ((Ord(S[I + 3]) and $3F) shl 12)
               + ((Ord(S[I + 4]) and $3F) shl 6)
               +  (Ord(S[I + 5]) and $3F);
      Inc(I, 5);
    end;

    if (CodePoint > Ord(High(WideChar))) then
      Result := Result + WideChar(HighSurrogate(CodePoint))
                       + WideChar(LowSurrogate(CodePoint))
    else
      Result := Result + WideChar(CodePoint and $FFFF);
    Inc(I);
  end;
end;

function LastPosW(const Needle, Haystack: WideString; Start: Integer = -1): Integer;
var
  I:        Integer;
  StartPos: Integer;
  TokenLen: Integer;
begin
  // Consider the substring of Haystack from the Start'th index to the right.
  // What is the rightmost occurence of Needle?

  Result := 0;
  TokenLen := Length(Needle);

  // Get starting position
  if Start < 0 then
    Start := Length(Haystack);

  StartPos := Length(Haystack) - TokenLen + 1;
  if (Start < StartPos) then
    StartPos := Start;

  // Search for the string
  for I := StartPos downto 1 do begin
    if (Copy(Haystack, I, TokenLen) = Needle) then begin
      Result := I;
      Break;
    end;
  end;
end;

function PosW(const Needle, Haystack: WideString): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Needle = '') or (Haystack = '') then Exit;

  I := 1;
  while (I <= Length(Haystack) - Length(Needle) + 1) do begin
    if (Copy(Haystack, I, Length(Needle)) = Needle) then begin
      Result := I;
      Break;
    end
    else begin
      // It would be nice here to increment I by the length of that substring
      // common between the search string at I and the substring. That means
      // we can't use Copy.
      Inc(I);
    end;
  end;
end;

function StringReplaceW(const S, OldPattern, NewPattern: WideString; ReplaceAll: Boolean = false): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  // We IGNORE rfIgnoreCase: case sensitivity is very complicated in Unicode,
  // and is relative to culture. For instance, "I" is NOT the upper case of "i"
  // in tr-TR (Turkey).

  // Code shamelessly based on SysUtils.StringReplace

  SearchStr := S;
  Patt := OldPattern;

  NewStr := S;
  Result := '';
  while (SearchStr <> '') do begin
    Offset := PosW(Patt, SearchStr);
    if (Offset = 0) then begin
      Result := Result + NewStr;
      Break;
    end;

    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);

    if not ReplaceAll then begin
      Result := Result + NewStr;
      Break;
    end;

    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

end.
