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
    Top = 41
    Width = 688
    Height = 412
    Align = alClient
    TabOrder = 0
  end
  object ServerType: TRadioGroup
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    Caption = ' Server type '
    Columns = 2
    Items.Strings = (
      'TCP'
      'UDP')
    TabOrder = 1
    OnClick = ServerTypeClick
  end
end
