object fmBasicClient: TfmBasicClient
  Left = 192
  Top = 103
  Width = 870
  Height = 640
  Caption = 'fmBasicClient'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 185
    Top = 0
    Width = 5
    Height = 613
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 190
    Top = 0
    Width = 672
    Height = 613
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 225
      Width = 672
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 672
      Height = 225
      Align = alTop
      TabOrder = 0
    end
    object Memo2: TMemo
      Left = 0
      Top = 230
      Width = 672
      Height = 383
      Align = alClient
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 613
    Align = alLeft
    BevelOuter = bvNone
    Color = clMaroon
    TabOrder = 1
  end
end
