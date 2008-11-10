object UIForm: TUIForm
  Left = 192
  Top = 107
  Width = 870
  Height = 640
  Caption = 'UIForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MainSplitter: TSplitter
    Left = 340
    Top = 24
    Width = 5
    Height = 589
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Call: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Call'
      TabOrder = 0
      OnClick = CallClick
    end
    object HangUp: TButton
      Left = 74
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Hang up'
      TabOrder = 1
      OnClick = HangUpClick
    end
    object CallerSendsMedia: TButton
      Left = 148
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Caller -->'
      Enabled = False
      TabOrder = 2
      OnClick = CallerSendsMediaClick
    end
    object CalleeSendsMedia: TButton
      Left = 222
      Top = 0
      Width = 75
      Height = 25
      Caption = '<-- Callee'
      Enabled = False
      TabOrder = 3
      OnClick = CalleeSendsMediaClick
    end
  end
  object PartyPanel: TPanel
    Left = 345
    Top = 24
    Width = 517
    Height = 589
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PartyPanelResize
    object PartySplitter: TSplitter
      Left = 185
      Top = 0
      Width = 5
      Height = 589
      Cursor = crHSplit
    end
    object CalleeLog: TMemo
      Left = 190
      Top = 0
      Width = 327
      Height = 589
      Align = alClient
      TabOrder = 0
    end
    object CallerLog: TMemo
      Left = 0
      Top = 0
      Width = 185
      Height = 589
      Align = alLeft
      TabOrder = 1
    end
  end
  object Log: TMemo
    Left = 0
    Top = 24
    Width = 340
    Height = 589
    Align = alLeft
    TabOrder = 2
  end
end
