object rnidSpike: TrnidSpike
  Left = 181
  Top = 197
  Width = 700
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
    Top = 35
    Width = 692
    Height = 418
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 448
      Top = 4
      Width = 71
      Height = 13
      Caption = 'Session Count:'
    end
    object SessionCounter: TLabel
      Left = 522
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label2: TLabel
      Left = 549
      Top = 4
      Width = 50
      Height = 13
      Caption = 'RTP bytes'
    end
    object RTPDataCount: TLabel
      Left = 604
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 547
      Top = 18
      Width = 51
      Height = 13
      Caption = 'UDP bytes'
    end
    object UDPDataCount: TLabel
      Left = 604
      Top = 18
      Width = 6
      Height = 13
      Caption = '0'
    end
    object TargetUri: TEdit
      Left = 76
      Top = 1
      Width = 290
      Height = 21
      TabOrder = 0
      Text = 'sip:computername;transport=udp'
    end
    object Invite: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Invite'
      TabOrder = 1
      OnClick = InviteClick
    end
    object Bye: TButton
      Left = 368
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Bye'
      TabOrder = 2
      OnClick = ByeClick
    end
  end
  object UiTimer: TTimer
    Interval = 100
    OnTimer = UiTimerTimer
    Left = 8
    Top = 40
  end
end
