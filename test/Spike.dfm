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
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 502
    Top = 35
    Width = 5
    Height = 418
    Cursor = crHSplit
    Align = alRight
  end
  object Log: TMemo
    Left = 0
    Top = 35
    Width = 502
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
  object Panel2: TPanel
    Left = 507
    Top = 35
    Width = 185
    Height = 418
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 25
      Height = 25
      Caption = '1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 40
      Top = 8
      Width = 25
      Height = 25
      Caption = '2'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 64
      Top = 8
      Width = 25
      Height = 25
      Caption = '3'
      TabOrder = 2
      OnClick = Button3Click
    end
    object ButtonA: TButton
      Left = 88
      Top = 8
      Width = 25
      Height = 25
      Caption = 'A'
      TabOrder = 10
      OnClick = ButtonAClick
    end
    object Button4: TButton
      Left = 16
      Top = 32
      Width = 25
      Height = 25
      Caption = '4'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 40
      Top = 32
      Width = 25
      Height = 25
      Caption = '5'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 64
      Top = 32
      Width = 25
      Height = 25
      Caption = '6'
      TabOrder = 5
      OnClick = Button6Click
    end
    object ButtonB: TButton
      Left = 88
      Top = 32
      Width = 25
      Height = 25
      Caption = 'B'
      TabOrder = 11
      OnClick = ButtonBClick
    end
    object Button7: TButton
      Left = 16
      Top = 56
      Width = 25
      Height = 25
      Caption = '7'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 40
      Top = 56
      Width = 25
      Height = 25
      Caption = '8'
      TabOrder = 7
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 64
      Top = 56
      Width = 25
      Height = 25
      Caption = '9'
      TabOrder = 8
      OnClick = Button9Click
    end
    object ButtonC: TButton
      Left = 88
      Top = 56
      Width = 25
      Height = 25
      Caption = 'C'
      TabOrder = 12
      OnClick = ButtonCClick
    end
    object ButtonStar: TButton
      Left = 16
      Top = 80
      Width = 25
      Height = 25
      Caption = '*'
      TabOrder = 14
      OnClick = ButtonStarClick
    end
    object Button0: TButton
      Left = 40
      Top = 80
      Width = 25
      Height = 25
      Caption = '0'
      TabOrder = 9
      OnClick = Button0Click
    end
    object ButtonHash: TButton
      Left = 64
      Top = 80
      Width = 25
      Height = 25
      Caption = '#'
      TabOrder = 15
      OnClick = ButtonHashClick
    end
    object ButtonD: TButton
      Left = 88
      Top = 80
      Width = 25
      Height = 25
      Caption = 'D'
      TabOrder = 13
      OnClick = ButtonDClick
    end
    object ButtonFlash: TButton
      Left = 16
      Top = 104
      Width = 97
      Height = 25
      Caption = 'Flash'
      TabOrder = 16
      OnClick = ButtonFlashClick
    end
  end
  object UiTimer: TTimer
    Interval = 100
    OnTimer = UiTimerTimer
    Left = 8
    Top = 40
  end
end
