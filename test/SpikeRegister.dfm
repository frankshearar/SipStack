object rnidSpikeRegister: TrnidSpikeRegister
  Left = 192
  Top = 174
  Width = 696
  Height = 480
  Caption = 'rnidSpikeRegister'
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
    object Register: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Register'
      TabOrder = 0
      OnClick = RegisterClick
    end
    object Query: TButton
      Left = 74
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Query'
      TabOrder = 1
      OnClick = QueryClick
    end
    object Unregister: TButton
      Left = 148
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Unregister'
      TabOrder = 2
      OnClick = UnregisterClick
    end
    object Registrar: TEdit
      Left = 224
      Top = 1
      Width = 249
      Height = 21
      TabOrder = 3
      Text = 'sip:wsjames;transport=udp'
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
