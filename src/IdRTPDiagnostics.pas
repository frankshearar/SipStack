{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRTPDiagnostics;

interface

uses
  Classes, Contnrs, ExtCtrls, Graphics, IdConnectionBindings, IdObservable,
  IdRTP, SyncObjs;

type
  TIdHistogramEntry = class(TObject)
  private
    fName:  String;
    fCount: Cardinal;
  public
    procedure IncrementCount;

    property Count: Cardinal read fCount write fCount;
    property Name:  String   read fName write fName;
  end;

  TIdHistogram = class(TPersistent)
  private
    List: TObjectList;

    procedure AddNewEntry(const Name: String);
    function  GetEntries(Index: Integer): TIdHistogramEntry;
    function  IndexOf(const Name: String): Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    procedure Clear;
    function  Count: Integer;
    function  HasEntry(const Name: String): Boolean;
    procedure RecordEvent(const Name: String);

    property  Entries[Index: Integer]: TIdHistogramEntry read GetEntries;
  end;

  TIdHistogramPanel = class(TPanel,
                            IIdObserver)
  private
    CurrentScale: Double;
    Data:         TIdHistogram;
    DataLock:     TCriticalSection;
    DefaultColor: TColor;
    DefaultScale: Double;
    fBarColor:    TColor;
    fLineSpacing: Integer;

    procedure DrawBar(const Name: String;
                      Magnitude: Integer;
                      NameWidth: Integer;
                      var X, Y: Integer);
    procedure DrawSeparator(X: Integer);
    function  GetMaxNameWidth: Integer;
    procedure LockData;
    procedure UnlockData;
  protected
    procedure OnChanged(Observed: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Reset;
  published
    property LineSpacing: Integer read fLineSpacing write fLineSpacing;
    property BarColor:    TColor  read fBarColor write fBarColor;
  end;

  TIdRTPPayloadHistogram = class(TIdObservable,
                                 IIdRTPListener)
  private
    Data: TIdHistogram;

    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure GetData(Data: TIdHistogram);
    procedure RecordEvent(const Name: String);
    function  RTPTypeCount: Integer;
  end;

const
  ItemNotFoundIndex = -1;  

implementation

uses
  Math, Types;

//******************************************************************************
//* TIdHistogramEntry                                                          *
//******************************************************************************
//* TIdHistogramEntry Public methods *******************************************

procedure TIdHistogramEntry.IncrementCount;
begin
  Inc(Self.fCount);
end;

//******************************************************************************
//* TIdHistogram                                                               *
//******************************************************************************
//* TIdHistogram Public methods ************************************************

constructor TIdHistogram.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdHistogram.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdHistogram.Assign(Src: TPersistent);
var
  I:     Integer;
  Other: TIdHistogram;
begin
  if (Src is TIdHistogram) then begin
    Other := Src as TIdHistogram;

    for I := 0 to Other.Count - 1 do begin
      Self.RecordEvent(Other.Entries[I].Name);
      Self.Entries[I].Count := Other.Entries[I].Count;
    end;
  end
  else
    inherited Assign(Src);
end;

procedure TIdHistogram.Clear;
begin
  Self.List.Clear;
end;

function TIdHistogram.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdHistogram.HasEntry(const Name: String): Boolean;
begin
  Result := Self.IndexOf(Name) <> ItemNotFoundIndex;
end;

procedure TIdHistogram.RecordEvent(const Name: String);
var
  Index: Integer;
begin
  Index := Self.IndexOf(Name);

  if (Index = ItemNotFoundIndex) then
    Self.AddNewEntry(Name)
  else
    Self.Entries[Index].IncrementCount;
end;

//* TIdHistogram Private methods ***********************************************

procedure TIdHistogram.AddNewEntry(const Name: String);
var
  NewEntry: TIdHistogramEntry;
begin
  NewEntry := TIdHistogramEntry.Create;
  try
    NewEntry.Count := 1;
    NewEntry.Name  := Name;

    Self.List.Add(NewEntry);
  except
    if (Self.List.IndexOf(NewEntry) <> ItemNotFoundIndex) then
      Self.List.Remove(NewEntry)
    else
      NewEntry.Free;

    raise;
  end;
end;

function TIdHistogram.GetEntries(Index: Integer): TIdHistogramEntry;
begin
  Result := Self.List[Index] as TIdHistogramEntry;
end;

function TIdHistogram.IndexOf(const Name: String): Integer;
begin
  Result := 0;

  while (Result < Self.Count) and (Self.Entries[Result].Name <> Name) do
    Inc(Result);

  if (Result = Self.Count) then
    Result := ItemNotFoundIndex;
end;

//******************************************************************************
//* TIdHistogramPanel                                                          *
//******************************************************************************
//* TIdHistogramPanel Public methods *******************************************

constructor TIdHistogramPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.BarColor     := clBlack;
  Self.DefaultScale := 10.0;
  Self.DefaultColor := clBlack;
  Self.Data         := TIdHistogram.Create;
  Self.DataLock     := TCriticalSection.Create;

  Self.CurrentScale := Self.DefaultScale;
  Self.LineSpacing  := 10;
end;

destructor TIdHistogramPanel.Destroy;
begin
  Self.LockData;
  try
    Self.Data.Free;
  finally
    Self.UnlockData;
  end;
  Self.DataLock.Free;

  inherited Destroy;
end;

procedure TIdHistogramPanel.Reset;
begin
end;

//* TIdHistogramPanel Protected methods ****************************************

procedure TIdHistogramPanel.OnChanged(Observed: TObject);
begin
  if not (Observed is TIdRTPPayloadHistogram) then Exit;

  Self.LockData;
  try
    (Observed as TIdRTPPayloadHistogram).GetData(Self.Data);
  finally
    Self.UnlockData;
  end;

  Self.Repaint;
end;

procedure TIdHistogramPanel.Paint;
var
  I:            Integer;
  MaxNameWidth: Integer;
  X:            Integer;
  Y:            Integer;
begin
  inherited Paint;

  Self.CurrentScale := Self.DefaultScale;
  Self.Canvas.Pen.Color := Self.DefaultColor;

  X := 0;
  Y := 0;
  MaxNameWidth := Self.GetMaxNameWidth;
  for I := 0 to Self.Data.Count - 1 do
    Self.DrawBar(Data.Entries[I].Name,
                 Data.Entries[I].Count,
                 MaxNameWidth,
                 X, Y);
end;

//* TIdHistogramPanel Private methods ******************************************

procedure TIdHistogramPanel.DrawBar(const Name: String;
                                    Magnitude: Integer;
                                    NameWidth: Integer;
                                    var X, Y: Integer);
var
  Bar:                TRect;
  BarBaseX:           Integer;
  BarWidth:           Integer;
  Height:             Integer;
  OriginalBrushColor: TColor;
  SeparatorX:         Integer;
begin
  Height := Self.Canvas.TextHeight(Name);

  Self.Canvas.TextOut(X, Y, Name);

  SeparatorX := NameWidth + 10;

  Self.DrawSeparator(SeparatorX);

  BarBaseX := SeparatorX + 10;

  repeat
    Self.CurrentScale := Self.CurrentScale / 2;
    BarWidth := Round(Self.CurrentScale*Magnitude);
  until (BarBaseX + BarWidth <= Self.Width);

  OriginalBrushColor := Self.Color;
  try
    Self.Canvas.Brush.Color := Self.BarColor;

    Bar.Left   := BarBaseX;
    Bar.Top    := Y;
    Bar.Right  := BarBaseX + BarWidth;
    Bar.Bottom := Y + Height;

    Self.Canvas.FillRect(Bar);
  finally
    Self.Canvas.Brush.Color := OriginalBrushColor;
  end;

  Inc(Y, Height + Self.LineSpacing);
end;

procedure TIdHistogramPanel.DrawSeparator(X: Integer);
var
  Height: Integer;
  Origin: TPoint;
  Start:  TPoint;
  TextHeight: Integer;
begin
  // Draw a vertical line for the current entry X pixels from the left of the
  // panel.
  Origin.X := 0;
  Origin.Y := 0;

  Start.X := X;
  Start.Y := 0;
  Self.Canvas.PenPos := Start;

  if (Self.Data.Count > 0) then
    TextHeight := Self.Canvas.TextHeight(Self.Data.Entries[0].Name)
  else
    TextHeight := 0;

  Height := TextHeight*Self.Data.Count + Self.LineSpacing*(Self.Data.Count - 1);

  Self.Canvas.LineTo(X, Height);
  Self.Canvas.PenPos := Origin;
end;

function TIdHistogramPanel.GetMaxNameWidth: Integer;
var
  I: Integer;
begin
  // How much horizontal space does the widest name in the histogram take up?

  Result := 0;
  for I := 0 to Self.Data.Count - 1 do
    Result := Max(Result, Self.Canvas.TextWidth(Self.Data.Entries[I].Name));
end;

procedure TIdHistogramPanel.LockData;
begin
  Self.DataLock.Acquire;
end;

procedure TIdHistogramPanel.UnlockData;
begin
  Self.DataLock.Release;
end;

//******************************************************************************
//* TIdRTPPayloadHistogram                                                     *
//******************************************************************************
//* TIdRTPPayloadHistogram Public methods **************************************

constructor TIdRTPPayloadHistogram.Create;
begin
  inherited Create;

  Self.Data := TIdHistogram.Create;
end;

destructor TIdRTPPayloadHistogram.Destroy;
begin
  Self.Data.Free;

  inherited Destroy;
end;

procedure TIdRTPPayloadHistogram.GetData(Data: TIdHistogram);
begin
  Data.Assign(Self.Data);
end;

procedure TIdRTPPayloadHistogram.RecordEvent(const Name: String);
begin
  Self.Data.RecordEvent(Name);
  Self.NotifyListenersOfChange;  
end;

function TIdRTPPayloadHistogram.RTPTypeCount: Integer;
begin
  Result := Self.Data.Count;
end;

//* TIdRTPPayloadHistogram Private methods *************************************

procedure TIdRTPPayloadHistogram.OnRTCP(Packet: TIdRTCPPacket;
                                        Binding: TIdConnectionBindings);
begin
end;

procedure TIdRTPPayloadHistogram.OnRTP(Packet: TIdRTPPacket;
                                       Binding: TIdConnectionBindings);
begin
  Self.RecordEvent(Packet.Payload.EncodingName);
end;

end.
