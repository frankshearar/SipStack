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
      Left = 416
      Top = 4
      Width = 71
      Height = 13
      Caption = 'Session Count:'
    end
    object SessionCounter: TLabel
      Left = 490
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object InviteSelf: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Invite Self'
      TabOrder = 0
      OnClick = InviteSelfClick
    end
  end
end
