unit DFFFilesForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin;

type
  TStrObj = class(TObject)
    strict private
      FValue: string;
    public
      constructor Create(aValue:string);

      property Value: string read FValue write FValue;
  end;

  TfrmDFFFiles = class(TForm)
    mmo1: TMemo;
    edtSearch: TEdit;
    seTolerance: TSpinEdit;
    pnl1: TPanel;
    procedure edtSearchChange(Sender: TObject);
  private
    { Private declarations }
    FFiles: TStringList;

    procedure DoFuzzySearch(aSearchInput: string);
  public
    { Public declarations }
    procedure SetFiles(aFiles: TStringList);
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
  var lMatches := TFuzzySearch.FuzzySearch(aSearchInput, FFiles.ToStringArray, seTolerance.Value);

  mmo1.Clear;
  for var lMatch in lMatches do
  begin
    mmo1.Lines.Add(lMatch);
  end;
end;

procedure TfrmDFFFiles.edtSearchChange(Sender: TObject);
begin
  DoFuzzySearch(edtSearch.Text);
end;

procedure TfrmDFFFiles.SetFiles(aFiles: TStringList);
begin
  FFiles := aFiles;

  for var lFile in aFiles do
  begin
    mmo1.Lines.Add(lFile);
  end;
end;

{ TStrObj }

constructor TStrObj.Create(aValue: string);
begin
  FValue := aValue;
end;

end.
