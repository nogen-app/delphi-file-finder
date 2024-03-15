unit FilesFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin,
  Vcl.StdCtrls, Vcl.ExtCtrls, ToolsAPI;

type
  TfrmFilesFrame = class(TFrame)
    lstFiles: TListBox;
    pnl1: TPanel;
    edtSearch: TEdit;
    seTolerance: TSpinEdit;
    procedure lstFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstFilesDblClick(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtSearchChange(Sender: TObject);
  private
    { Private declarations }
    FFiles: TStringList;
    FOnEscCalled: TNotifyEvent;

    procedure DoFuzzySearch(aSearchInput: string);
    procedure OpenSelectedFile;
  public
    { Public declarations }

    procedure SetFiles(aFiles: TStringList);

    property OnEscCalled: TNotifyEvent read FOnEscCalled write FOnEscCalled;

  end;

implementation

uses
  FuzzySearch, Helpers;

{$R *.dfm}

procedure TfrmFilesFrame.DoFuzzySearch(aSearchInput: string);
begin
  var lMatches := TFuzzySearch.FuzzySearch(aSearchInput, FFiles, seTolerance.Value);

  lstFiles.Clear;
  var idx: Integer := 0;
  for var lFile in lMatches do
  begin
    lstFiles.AddItem(lFile, lMatches.Objects[idx]);
    Inc(idx);
  end;

  if lstFiles.Count > 0 then
    lstFiles.Selected[0] := True;
end;

procedure TfrmFilesFrame.edtSearchChange(Sender: TObject);
begin
  DoFuzzySearch(edtSearch.Text);
end;

procedure TfrmFilesFrame.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenSelectedFile;

  if (Key = VK_ESCAPE) and Assigned(FOnEscCalled) then
    FOnEscCalled(Self);
end;

procedure TfrmFilesFrame.lstFilesDblClick(Sender: TObject);
begin
  OpenSelectedFile;
end;

procedure TfrmFilesFrame.lstFilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenSelectedFile;

  if (Key = VK_ESCAPE) and Assigned(FOnEscCalled) then
    FOnEscCalled(Self);
end;

procedure TfrmFilesFrame.OpenSelectedFile;
begin
  var lSelectedFile := TStrObj(lstFiles.Items.Objects[lstFiles.ItemIndex]).Value;

  var lActionService: IOTAActionServices;
  if Supports(BorlandIDEServices, IOTAActionServices, lActionService) then
  begin
    lActionService.OpenFile(lSelectedFile);
  end;
end;

procedure TfrmFilesFrame.SetFiles(aFiles: TStringList);
begin
  FFiles := aFiles;

  var idx: Integer := 0;
  for var lFile in aFiles do
  begin
    lstFiles.AddItem(lFile, aFiles.Objects[idx]);
    Inc(idx);
  end;
end;

end.
