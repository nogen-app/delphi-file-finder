unit FilesFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin,
  Vcl.StdCtrls, Vcl.ExtCtrls, ToolsAPI,
  System.Generics.Collections, Spring.Container,
  Services.IRepo;

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
    FOnEscCalled: TNotifyEvent;

    procedure DoCachedSearch(aSearchInput: string);
    procedure OpenSelectedFile;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;

    property OnEscCalled: TNotifyEvent read FOnEscCalled write FOnEscCalled;
  end;

implementation

uses
  System.IOUtils;

{$R *.dfm}

//TODO: Show what parts of the string it matched on
constructor TfrmFilesFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TfrmFilesFrame.DoCachedSearch(aSearchInput: string);
begin
  var lMatches := GlobalCOntainer.Resolve<IRepo>.GetFiles(aSearchInput);
  lstFiles.Clear;

  for var lFile in lMatches do
  begin
    lstFiles.AddItem(lFile.Path, nil);
  end;

  if lstFiles.Count > 0 then
    lstFiles.Selected[0] := True;

  FreeAndNil(lMatches);
end;

procedure TfrmFilesFrame.edtSearchChange(Sender: TObject);
begin
  DoCachedSearch(edtSearch.Text);
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
  if lstFiles.Count = 0 then exit;
  
  var lSelectedFile := lstFiles.Items[lstFiles.ItemIndex];
  var lRootPath := ExtractFilePath(GetActiveProject.FileName);
  lSelectedFile := TPath.GetFullPath(TPath.Combine(lRootPath, lSelectedFile)); 

  var lActionService: IOTAActionServices;
  if Supports(BorlandIDEServices, IOTAActionServices, lActionService) then
  begin
    lActionService.OpenFile(lSelectedFile);
  end;
end;

end.
