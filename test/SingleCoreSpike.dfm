object SingleCore: TSingleCore
  Left = 192
  Top = 103
  Width = 870
  Height = 640
  Caption = 'SingleCore'
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
    Left = 393
    Top = 41
    Width = 5
    Height = 572
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object MessageLog: TMemo
    Left = 398
    Top = 41
    Width = 464
    Height = 572
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 393
    Height = 572
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 0
      Top = 128
      Width = 125
      Height = 13
      Caption = 'Local Session Description:'
    end
    object Label2: TLabel
      Left = 192
      Top = 128
      Width = 136
      Height = 13
      Caption = 'Remote Session Description:'
    end
    object Label3: TLabel
      Left = 0
      Top = 240
      Width = 65
      Height = 13
      Caption = 'Configuration:'
    end
    object ReferredResourceLabel: TLabel
      Left = 80
      Top = 78
      Width = 3
      Height = 13
    end
    object ToHeader: TEdit
      Left = 78
      Top = 2
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object Call: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Action = CallAction
      TabOrder = 1
    end
    object HangUp: TButton
      Left = 0
      Top = 24
      Width = 75
      Height = 25
      Action = HangUpAction
      TabOrder = 2
    end
    object LocalSessionDescription: TMemo
      Left = 0
      Top = 144
      Width = 185
      Height = 89
      Lines.Strings = (
        'v=0'
        'o=fronk 0 0 IN IP4 127.0.0.1'
        's=Te(s|x)t'
        'c=IN IP4 127.0.0.1'
        't=0 0'
        'm=text 0 RTP/AVP 98'
        'a=rtpmap:98 t140/1000')
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object RemoteSessionDescription: TMemo
      Left = 192
      Top = 144
      Width = 185
      Height = 89
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 4
    end
    object Configuration: TMemo
      Left = 0
      Top = 256
      Width = 273
      Height = 89
      Lines.Strings = (
        'NameServer: MOCK'
        'Listen: UDP 127.0.0.1 xxxx'
        'Listen: TCP 127.0.0.1 xxxx'
        'From: "Count Zero" <sip:case@127.0.0.1:xxxx>'
        'Contact: "Count Zero" <sip:case@127.0.0.1:xxxx>'
        'SupportEvent: refer')
      ScrollBars = ssVertical
      TabOrder = 5
    end
    object RefreshConfig: TButton
      Left = 0
      Top = 352
      Width = 113
      Height = 25
      Action = RefreshConfigAction
      TabOrder = 6
    end
    object Answer: TButton
      Left = 75
      Top = 24
      Width = 75
      Height = 25
      Action = AnswerAction
      TabOrder = 7
    end
    object Transfer: TButton
      Left = 0
      Top = 48
      Width = 75
      Height = 25
      Action = TransferAction
      TabOrder = 8
    end
    object ReferredResource: TEdit
      Left = 78
      Top = 50
      Width = 121
      Height = 21
      TabOrder = 9
    end
    object FollowRefer: TButton
      Left = 0
      Top = 72
      Width = 75
      Height = 25
      Action = FollowReferAction
      TabOrder = 10
    end
  end
  object ActionManager: TActionManager
    Left = 248
    Top = 56
    object CallAction: TAction
      Caption = 'Call'
      OnExecute = CallActionExecute
    end
    object HangUpAction: TAction
      Caption = 'Hang Up'
      OnExecute = HangUpActionExecute
    end
    object RefreshConfigAction: TAction
      Caption = 'Refresh Configuration'
    end
    object AnswerAction: TAction
      Caption = 'Answer'
      OnExecute = AnswerActionExecute
    end
    object TransferAction: TAction
      Caption = 'Transfer'
      OnExecute = TransferActionExecute
    end
    object FollowReferAction: TAction
      Caption = 'Follow Refer'
      OnExecute = FollowReferActionExecute
    end
  end
end
