object Spike: TSpike
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'SIP Test Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 26
    Align = alTop
    TabOrder = 0
    object Invite: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Invite'
      TabOrder = 0
      OnClick = InviteClick
    end
    object Bye: TButton
      Left = 75
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Bye'
      TabOrder = 1
      OnClick = ByeClick
    end
  end
  object Log: TMemo
    Left = 0
    Top = 26
    Width = 688
    Height = 427
    Align = alClient
    TabOrder = 1
  end
end
