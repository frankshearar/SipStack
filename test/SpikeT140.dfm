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
    Top = 114
    Width = 688
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object Sent: TMemo
    Left = 0
    Top = 25
    Width = 688
    Height = 89
    Align = alTop
    TabOrder = 0
  end
  object Received: TMemo
    Left = 0
    Top = 119
    Width = 688
    Height = 334
    Align = alClient
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 304
      Top = 4
      Width = 54
      Height = 13
      Caption = 'Byte count:'
    end
    object ByteCount: TLabel
      Left = 360
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label2: TLabel
      Left = 408
      Top = 4
      Width = 68
      Height = 13
      Caption = 'Packet Count:'
    end
    object PacketCount: TLabel
      Left = 478
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Send: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Send'
      TabOrder = 0
      OnClick = SendClick
    end
    object SendFile: TEdit
      Left = 76
      Top = 2
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '..\etc\bigburp.wav'
    end
    object Save: TButton
      Left = 199
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 2
      OnClick = SaveClick
    end
  end
end
