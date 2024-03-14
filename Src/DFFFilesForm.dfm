object frmDFFFiles: TfrmDFFFiles
  Left = 0
  Top = 0
  Caption = 'frmDFFFiles'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object mmo1: TMemo
    Left = 0
    Top = 0
    Width = 624
    Height = 414
    Align = alClient
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 414
    Width = 624
    Height = 27
    Align = alBottom
    Caption = 'pnl1'
    TabOrder = 1
    object edtSearch: TEdit
      Left = 1
      Top = 1
      Width = 567
      Height = 25
      Align = alClient
      TabOrder = 0
      Text = 'edtSearch'
      OnChange = edtSearchChange
      ExplicitHeight = 23
    end
    object seTolerance: TSpinEdit
      Left = 568
      Top = 1
      Width = 55
      Height = 25
      Align = alRight
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 3
    end
  end
end
