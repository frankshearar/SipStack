object rnidSpike: TrnidSpike
  Left = 277
  Top = 155
  Width = 932
  Height = 505
  Caption = 'rnidSpike'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 602
    Top = 0
    Width = 5
    Height = 478
    Cursor = crHSplit
    Align = alRight
  end
  object IOPanel: TPanel
    Left = 607
    Top = 0
    Width = 317
    Height = 478
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object InputSplitter: TSplitter
      Left = 0
      Top = 169
      Width = 317
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object UpperInput: TPanel
      Left = 0
      Top = 0
      Width = 317
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
        Width = 312
        Height = 169
        Align = alClient
        TabOrder = 0
        OnKeyPress = InputTextKeyPress
      end
    end
    object LowerInput: TPanel
      Left = 0
      Top = 174
      Width = 317
      Height = 304
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter4: TSplitter
        Left = 0
        Top = 0
        Width = 5
        Height = 304
        Cursor = crHSplit
        Color = clBtnFace
        ParentColor = False
      end
      object OutputText: TMemo
        Left = 5
        Top = 0
        Width = 312
        Height = 304
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object DebugPanel: TPanel
    Left = 0
    Top = 0
    Width = 602
    Height = 478
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinWidth = 530
    TabOrder = 1
    object Log: TMemo
      Left = 0
      Top = 146
      Width = 602
      Height = 332
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 602
      Height = 146
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        602
        146)
      object Label1: TLabel
        Left = 516
        Top = 2
        Width = 71
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Session Count:'
      end
      object SessionCounter: TLabel
        Left = 590
        Top = 2
        Width = 6
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '0'
      end
      object Label2: TLabel
        Left = 535
        Top = 18
        Width = 50
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'RTP bytes'
      end
      object RTPDataCount: TLabel
        Left = 590
        Top = 18
        Width = 6
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '0'
      end
      object Label3: TLabel
        Left = 533
        Top = 34
        Width = 51
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'UDP bytes'
      end
      object UDPDataCount: TLabel
        Left = 590
        Top = 34
        Width = 6
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '0'
      end
      object Label4: TLabel
        Left = 34
        Top = 76
        Width = 40
        Height = 13
        Caption = 'Contact:'
      end
      object Label5: TLabel
        Left = 369
        Top = 100
        Width = 49
        Height = 13
        Caption = 'Base Port:'
      end
      object Label6: TLabel
        Left = 50
        Top = 101
        Width = 25
        Height = 13
        Caption = 'Host:'
      end
      object Label7: TLabel
        Left = 46
        Top = 124
        Width = 29
        Height = 13
        Caption = 'Proxy:'
      end
      object TargetUri: TEdit
        Left = 76
        Top = 1
        Width = 290
        Height = 21
        TabOrder = 0
        Text = 'sip:frank@roke.angband.za.org'
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
      object BasePort: TEdit
        Left = 292
        Top = 97
        Width = 74
        Height = 21
        Hint = 'This stack'#39's local port'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Text = '5060'
        OnChange = BasePortChange
      end
      object Options: TButton
        Left = 368
        Top = 0
        Width = 75
        Height = 25
        Caption = 'Options'
        TabOrder = 5
        OnClick = OptionsClick
      end
      object UseAsProxy: TCheckBox
        Left = 202
        Top = 122
        Width = 83
        Height = 17
        Hint = 'If checked, use the IP to the left as a proxy'
        Caption = 'Use as proxy'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = UseAsProxyClick
      end
      object UseLooseRouting: TCheckBox
        Left = 285
        Top = 124
        Width = 50
        Height = 17
        Hint = 'Use loose routing (RFC 3261)'
        Caption = 'Use lr'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 7
      end
      object ContactUri: TEdit
        Left = 77
        Top = 73
        Width = 289
        Height = 21
        TabOrder = 9
        Text = 'sip:franks@80.168.137.82'
        OnChange = ContactUriChange
      end
      object HostName: TEdit
        Left = 77
        Top = 97
        Width = 121
        Height = 21
        TabOrder = 10
        Text = '80.168.137.82'
        OnChange = HostNameChange
      end
      object MasqAsNat: TCheckBox
        Left = 203
        Top = 100
        Width = 87
        Height = 17
        Hint = 'If checked, use the IP to the left in the SDP payloads'
        Caption = 'Masq as NAT'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 11
      end
      object Answer: TButton
        Left = 0
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Answer'
        Enabled = False
        TabOrder = 12
        OnClick = AnswerClick
      end
      object Register: TButton
        Left = 368
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Register'
        TabOrder = 4
        OnClick = RegisterClick
      end
      object Unregister: TButton
        Left = 368
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Unregister'
        TabOrder = 3
        OnClick = UnregisterClick
      end
      object Bye: TButton
        Left = 0
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Bye'
        TabOrder = 2
        OnClick = ByeClick
      end
      object Edit1: TEdit
        Left = 77
        Top = 121
        Width = 121
        Height = 21
        TabOrder = 13
        OnChange = HostNameChange
      end
    end
  end
  object Password: TEdit
    Left = 76
    Top = 25
    Width = 290
    Height = 21
    TabOrder = 2
    Text = 'password'
    OnChange = PasswordChange
  end
  object UiTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = UiTimerTimer
    Left = 128
    Top = 224
  end
  object TextTimer: TTimer
    Interval = 300
    OnTimer = TextTimerTimer
    Left = 160
    Top = 224
  end
end
