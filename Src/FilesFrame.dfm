object frmFilesFrame: TfrmFilesFrame
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object lstFiles: TListBox
    Left = 0
    Top = 0
    Width = 640
    Height = 453
    Align = alClient
    ItemHeight = 15
    TabOrder = 0
    OnDblClick = lstFilesDblClick
    OnKeyUp = lstFilesKeyUp
    ExplicitWidth = 624
    ExplicitHeight = 414
  end
  object pnl1: TPanel
    Left = 0
    Top = 453
    Width = 640
    Height = 27
    Align = alBottom
    Caption = 'pnl1'
    TabOrder = 1
    ExplicitTop = 414
    ExplicitWidth = 624
    object edtSearch: TEdit
      Left = 1
      Top = 1
      Width = 583
      Height = 25
      Align = alClient
      TabOrder = 0
      Text = 'edtSearch'
      OnChange = edtSearchChange
      OnKeyUp = edtSearchKeyUp
      ExplicitWidth = 567
      ExplicitHeight = 23
    end
    object seTolerance: TSpinEdit
      Left = 584
      Top = 1
      Width = 55
      Height = 25
      Align = alRight
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 3
      ExplicitLeft = 568
    end
  end
end
