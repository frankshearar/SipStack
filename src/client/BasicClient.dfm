object fmBasicClient: TfmBasicClient
  Left = 94
  Top = 187
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
  object Splitter1: TSplitter
    Left = 0
    Top = 30
    Width = 862
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object Holder: TPanel
    Left = 0
    Top = 35
    Width = 862
    Height = 578
    Align = alClient
    BevelOuter = bvNone
    Color = clBlue
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Event: TEdit
      Left = 4
      Top = 4
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'foo'
    end
    object Trigger: TButton
      Left = 130
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Trigger'
      Default = True
      TabOrder = 1
      OnClick = TriggerClick
    end
  end
end
