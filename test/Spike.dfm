object rnidSpike: TrnidSpike
  Left = 277
  Top = 188
  Width = 928
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 547
    Top = 0
    Width = 5
    Height = 453
    Cursor = crHSplit
    Align = alRight
  end
  object IOPanel: TPanel
    Left = 552
    Top = 0
    Width = 368
    Height = 453
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object InputSplitter: TSplitter
      Left = 0
      Top = 169
      Width = 368
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object UpperInput: TPanel
      Left = 0
      Top = 0
      Width = 368
      Height = 169
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter3: TSplitter
        Left = 0
        Top = 0
        Width = 5
        Height = 169
        Cursor = crHSplit
        Color = clBtnFace
        ParentColor = False
      end
      object InputText: TMemo
        Left = 5
        Top = 0
        Width = 363
        Height = 169
        Align = alClient
        TabOrder = 0
        OnKeyPress = InputTextKeyPress
      end
    end
    object LowerInput: TPanel
      Left = 0
      Top = 174
      Width = 368
      Height = 279
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter4: TSplitter
        Left = 0
        Top = 0
        Width = 5
        Height = 279
        Cursor = crHSplit
        Color = clBtnFace
        ParentColor = False
      end
      object OutputText: TMemo
        Left = 5
        Top = 0
        Width = 363
        Height = 279
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 547
    Height = 453
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Log: TMemo
      Left = 0
      Top = 89
      Width = 547
      Height = 364
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 547
      Height = 89
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 368
        Top = 26
        Width = 71
        Height = 13
        Caption = 'Session Count:'
      end
      object SessionCounter: TLabel
        Left = 442
        Top = 26
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label2: TLabel
        Left = 387
        Top = 42
        Width = 50
        Height = 13
        Caption = 'RTP bytes'
      end
      object RTPDataCount: TLabel
        Left = 442
        Top = 42
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label3: TLabel
        Left = 385
        Top = 58
        Width = 51
        Height = 13
        Caption = 'UDP bytes'
      end
      object UDPDataCount: TLabel
        Left = 442
        Top = 58
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
        Text = 'sip:fasil01@193.116.120.160'
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
      object BasePort: TEdit
        Left = 445
        Top = 1
        Width = 74
        Height = 21
        TabOrder = 3
        Text = 'BasePort'
        OnChange = BasePortChange
      end
    end
  end
  object Register: TButton
    Left = 0
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Register'
    TabOrder = 2
    OnClick = RegisterClick
  end
  object RegistrarUri: TEdit
    Left = 76
    Top = 25
    Width = 290
    Height = 21
    TabOrder = 3
    Text = 'sip:wsjames;transport=udp'
  end
  object UiTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = UiTimerTimer
    Left = 584
    Top = 8
  end
  object TextTimer: TTimer
    Interval = 300
    OnTimer = TextTimerTimer
    Left = 616
    Top = 8
  end
end
