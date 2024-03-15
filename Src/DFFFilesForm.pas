unit DFFFilesForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, Vcl.FileCtrl;

type
  TStrObj = class(TObject)
    strict private
      FValue: string;
    public
      constructor Create(aValue:string);

      property Value: string read FValue write FValue;
  end;

  TfrmDFFFiles = class(TForm)
    edtSearch: TEdit;
    seTolerance: TSpinEdit;
    pnl1: TPanel;
    lstFiles: TListBox;
    procedure edtSearchChange(Sender: TObject);
    procedure lstFilesDblClick(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FFiles: TStringList;
    FSelectedFile: TFileName;

    procedure DoFuzzySearch(aSearchInput: string);
    procedure OpenSelectedFile;
  public
    { Public declarations }
    procedure SetFiles(aFiles: TStringList);

    property SelectedFile: TFileName read FSelectedFile write FSelectedFile;
  end;

implementation

uses
  FuzzySearch;

{$R *.dfm}

{ TfrmDFFFiles }

//TODO:
// 1. Make sure the design matches delphi
// 2. Let the user select the file, and enter to open
// 3. Let the user press escape to exit the form

procedure TfrmDFFFiles.DoFuzzySearch(aSearchInput: string);
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

procedure TfrmDFFFiles.edtSearchChange(Sender: TObject);
begin
  DoFuzzySearch(edtSearch.Text);
end;

procedure TfrmDFFFiles.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenSelectedFile;

  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TfrmDFFFiles.lstFilesDblClick(Sender: TObject);
begin
  OpenSelectedFile;
end;

procedure TfrmDFFFiles.lstFilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenSelectedFile;

  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TfrmDFFFiles.OpenSelectedFile;
begin
  FSelectedFile := TStrObj(lstFiles.Items.Objects[lstFiles.ItemIndex]).Value;
  ModalResult := mrOk;
end;

procedure TfrmDFFFiles.SetFiles(aFiles: TStringList);
begin
  FFiles := aFiles;

  var idx: Integer := 0;
  for var lFile in aFiles do
  begin
    lstFiles.AddItem(lFile, aFiles.Objects[idx]);
    Inc(idx);
  end;
end;

{ TStrObj }

constructor TStrObj.Create(aValue: string);
begin
  FValue := aValue;
end;

end.
