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

    procedure DoWinAPISearch(aSearchInput: string);
    procedure DoFuzzySearch(aSearchInput: string);
    procedure DoCachedSearch(aSearchInput: string);
    procedure OpenSelectedFile;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;

    procedure SetFiles(aFiles: TList<string>);

    property OnEscCalled: TNotifyEvent read FOnEscCalled write FOnEscCalled;

  end;

implementation

uses
  FuzzySearch, System.IOUtils;

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

procedure TfrmFilesFrame.DoFuzzySearch(aSearchInput: string);
begin
  //Should only search if something has been entered, otherwise we show all files
//  if aSearchInput <> '' then
//  begin
//    var lMatches := TFuzzySearch.FuzzySearch(aSearchInput, FFiles, seTolerance.Value);
//
//    lstFiles.Clear;
//    for var lFile in lMatches do
//    begin
//      lstFiles.AddItem(lFile.Value, nil);
//    end;
//
//    if lstFiles.Count > 0 then
//      lstFiles.Selected[0] := True;
//  end else
//  begin
//    lstFiles.Clear;
//    for var lFile in FFiles do
//      lstFiles.AddItem(lFile, nil);
//  end;
end;

procedure TfrmFilesFrame.DoWinAPISearch(aSearchInput: string);
begin
  var lDirectories := TList<string>.create;

  var lProject := GetActiveProject;
  if not Assigned(lProject) then //Should only try and find files, if a project is currently active. Exit early otherwise
    Exit;

  var lProjectPath := ExtractFilePath(lProject.FileName);

  var lFiles := TList<string>.Create;

  var lTmpFiles := TStringList.Create;
  lProject.GetCompleteFileList(lTmpFiles);

  for var lFile in lTmpFiles do
  begin
    if not lDirectories.Contains(ExtractFileDir(lFile)) then
      lDirectories.Add(ExtractFileDir(lFile));
  end;

  FreeAndNil(lTmpFiles);

  var lSearchUnits := TStringList.Create;
  var lConf := lProject.ProjectOptions as IOTAProjectOptionsConfigurations;

  var lPlatform := lConf.ActiveConfiguration;
  lPlatform.GetValues('DCC_UnitSearchPath', lSearchUnits);

  for var lSearchUnit in lSearchUnits do
  begin
    var lPath :TFileName;

    lPath := TPath.GetFullPath(TPath.Combine(lProjectPath, lSearchUnit));

    if TDirectory.Exists(lPath) then
    begin
      if not lDirectories.Contains(lPath) then
        lDirectories.Add(lPath);
    end;
  end;

  lstFiles.Clear;

  var lpFindFileData: Win32_Find_Data;

  for var lDirectory in lDirectories do
  begin
    var lHandle := FindFirstFileEx(PWideChar(lDirectory+'\*'+aSearchInput+'*.pas'), FindExInfoBasic, @lpFindFileData, FindExSearchNameMatch, nil, 0);
    if lHandle <> INVALID_HANDLE_VALUE then
    begin
      var lMoreFiles: boolean := true;

      while lMoreFiles do
      begin
        var lFullPath := TPath.Combine(lDirectory, lpFindFileData.cFileName);
        var lRelativePath := ExtractRelativePath(lProjectPath, lFullPath);
        lstFiles.AddItem(lRelativePath, nil);

        lMoreFiles := FindNextFile(lHandle, lpFindFileData);
      end;
      Winapi.Windows.FindClose(lHandle);
    end else
    begin
      var lErr := GetLastError;
      if lErr <> ERROR_FILE_NOT_FOUND then
        ShowMessage(SysErrorMessage(lErr));
    end;
  end;
end;

procedure TfrmFilesFrame.edtSearchChange(Sender: TObject);
begin
//  DoFuzzySearch(edtSearch.Text);
//  DoWinAPISearch(edtSearch.Text);
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

procedure TfrmFilesFrame.SetFiles(aFiles: TList<string>);
begin
//  FFiles.Clear;
//  for var lFile in aFiles do
//  begin
//    FFiles.Add(lFile);
//  end;
//
//  lstFiles.Clear;
//
//  for var lFile in FFiles do
//  begin
//    lstFiles.AddItem(lFile, nil);
//  end;
//
//  DoFuzzySearch(edtSearch.Text);
end;

end.
