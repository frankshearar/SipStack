object Form1: TForm1
  Left = 192
  Top = 106
  Width = 696
  Height = 480
  Caption = 'Form1'
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
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Invite: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Invite'
      TabOrder = 0
    end
    object Bye: TButton
      Left = 272
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Bye'
      TabOrder = 1
    end
    object Target: TEdit
      Left = 76
      Top = 2
      Width = 195
      Height = 21
      TabOrder = 2
      Text = 'sip:computername;transport=udp'
    end
  end
end
