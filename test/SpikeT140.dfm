object IdSpikeT140: TIdSpikeT140
  Left = 192
  Top = 103
  Width = 696
  Height = 480
  Caption = 'IdSpikeT140'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 89
    Width = 688
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object Sent: TMemo
    Left = 0
    Top = 0
    Width = 688
    Height = 89
    Align = alTop
    TabOrder = 0
    OnKeyPress = SentKeyPress
  end
  object Received: TMemo
    Left = 0
    Top = 94
    Width = 688
    Height = 359
    Align = alClient
    TabOrder = 1
  end
end
