object rnidSpikeRegistrar: TrnidSpikeRegistrar
  Left = 192
  Top = 174
  Width = 696
  Height = 480
  Caption = 'rnidSpikeRegistrar'
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
    Left = 498
    Top = 24
    Width = 5
    Height = 429
    Cursor = crHSplit
    Align = alRight
  end
  object Log: TMemo
    Left = 0
    Top = 24
    Width = 498
    Height = 429
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 3
      Top = 5
      Width = 22
      Height = 13
      Caption = 'Port:'
    end
    object Port: TEdit
      Left = 27
      Top = 1
      Width = 37
      Height = 21
      TabOrder = 0
      Text = '25060'
      OnChange = PortChange
    end
  end
  object Contacts: TMemo
    Left = 503
    Top = 24
    Width = 185
    Height = 429
    Align = alRight
    TabOrder = 2
  end
end
