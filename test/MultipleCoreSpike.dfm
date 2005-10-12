object MultiCore: TMultiCore
  Left = 192
  Top = 103
  Width = 870
  Height = 640
  Caption = 'MultiCore'
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
  object VertSplitter: TSplitter
    Left = 425
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
    object RecreateUAs: TButton
      Left = -8
      Top = 0
      Width = 89
      Height = 25
      Caption = 'Recreate UAs'
      TabOrder = 0
      OnClick = RecreateUAsClick
    end
  end
  object RightPanel: TPanel
    Left = 430
    Top = 41
    Width = 432
    Height = 572
    Align = alClient
    BevelOuter = bvNone
    Color = clGreen
    TabOrder = 1
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 41
    Width = 425
    Height = 572
    Align = alLeft
    BevelOuter = bvNone
    Color = clRed
    TabOrder = 2
  end
end
