object rnidSpike: TrnidSpike
  Left = 277
  Top = 167
  Width = 928
  Height = 505
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
    Left = 535
    Top = 0
    Width = 5
    Height = 478
    Cursor = crHSplit
    Align = alRight
  end
  object IOPanel: TPanel
    Left = 540
    Top = 0
    Width = 380
    Height = 478
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object InputSplitter: TSplitter
      Left = 0
      Top = 169
      Width = 380
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object UpperInput: TPanel
      Left = 0
      Top = 0
      Width = 380
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
        Width = 375
        Height = 169
        Align = alClient
        TabOrder = 0
        OnKeyPress = InputTextKeyPress
      end
    end
    object LowerInput: TPanel
      Left = 0
      Top = 174
      Width = 380
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
        Width = 375
        Height = 304
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object DebugPanel: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 478
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinWidth = 535
    TabOrder = 1
    object Log: TMemo
      Left = 0
      Top = 99
      Width = 535
      Height = 379
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 535
      Height = 99
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 448
        Top = 2
        Width = 71
        Height = 13
        Caption = 'Session Count:'
      end
      object SessionCounter: TLabel
        Left = 522
        Top = 2
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label2: TLabel
        Left = 467
        Top = 18
        Width = 50
        Height = 13
        Caption = 'RTP bytes'
      end
      object RTPDataCount: TLabel
        Left = 522
        Top = 18
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label3: TLabel
        Left = 465
        Top = 34
        Width = 51
        Height = 13
        Caption = 'UDP bytes'
      end
      object UDPDataCount: TLabel
        Left = 522
        Top = 34
        Width = 6
        Height = 13
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
        Left = 241
        Top = 52
        Width = 49
        Height = 13
        Caption = 'Base Port:'
      end
      object Label6: TLabel
        Left = 382
        Top = 53
        Width = 25
        Height = 13
        Caption = 'Host:'
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
        Left = 292
        Top = 49
        Width = 74
        Height = 21
        TabOrder = 8
        Text = '5060'
        OnChange = BasePortChange
      end
      object Register: TButton
        Left = 0
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Register'
        TabOrder = 4
        OnClick = RegisterClick
      end
      object Unregister: TButton
        Left = 0
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Unregister'
        TabOrder = 3
        OnClick = UnregisterClick
      end
      object Options: TButton
        Left = 368
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Options'
        TabOrder = 5
        OnClick = OptionsClick
      end
      object UseAsProxy: TCheckBox
        Left = 80
        Top = 52
        Width = 97
        Height = 17
        Caption = 'Use as proxy'
        TabOrder = 6
        OnClick = UseAsProxyClick
      end
      object UseLooseRouting: TCheckBox
        Left = 164
        Top = 52
        Width = 50
        Height = 17
        Caption = 'Use lr'
        Checked = True
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
        Left = 409
        Top = 49
        Width = 121
        Height = 21
        TabOrder = 10
        Text = '80.168.137.82'
        OnChange = HostNameChange
      end
      object MasqAsNat: TCheckBox
        Left = 410
        Top = 73
        Width = 87
        Height = 17
        Caption = 'Masq as NAT'
        Checked = True
        State = cbChecked
        TabOrder = 11
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
