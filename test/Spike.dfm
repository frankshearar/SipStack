object rnidSpike: TrnidSpike
  Left = 192
  Top = 103
  Width = 696
  Height = 480
  Caption = 'rnidSpike'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Log: TMemo
    Left = 0
    Top = 24
    Width = 688
    Height = 429
    Align = alClient
    ScrollBars = ssVertical
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
      Left = 5
      Top = 4
      Width = 71
      Height = 13
      Caption = 'Session Count:'
    end
    object SessionCounter: TLabel
      Left = 79
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label2: TLabel
      Left = 490
      Top = 4
      Width = 57
      Height = 13
      Caption = 'Data Count:'
    end
    object DataCount: TLabel
      Left = 550
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
  end
  object UiTimer: TTimer
    Interval = 100
    OnTimer = UiTimerTimer
    Left = 216
    Top = 8
  end
end
