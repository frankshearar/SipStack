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
    OnKeyPress = SentKeyPress
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
      Left = 424
      Top = 4
      Width = 54
      Height = 13
      Caption = 'Byte count:'
    end
    object ByteCount: TLabel
      Left = 480
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label2: TLabel
      Left = 528
      Top = 4
      Width = 68
      Height = 13
      Caption = 'Packet Count:'
    end
    object PacketCount: TLabel
      Left = 598
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Join: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Join'
      TabOrder = 0
      OnClick = JoinClick
    end
    object RemoteHostAndPort: TEdit
      Left = 76
      Top = 2
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '127.0.0.1:8000'
    end
    object Leave: TButton
      Left = 336
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Leave'
      TabOrder = 2
      OnClick = LeaveClick
    end
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 528
    Top = 16
  end
end
