unit FilesIndexingThread;

interface

uses
  ToolsAPI, System.Classes, System.SyncObjs, System.Generics.Collections,
  Helpers;

type
  TFilesIndexingThread = class(TThread)
    private
      FFiles: TThreadList<TFileObj>;
      FEvent: TEvent;

      procedure IndexFiles;

      function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
    protected
      procedure Execute; override;
    public
      constructor Create(aFiles: TThreadList<TFileObj>);
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TFilesIndexingThread }

constructor TFilesIndexingThread.Create(aFiles: TThreadList<TFileObj>);
begin
  inherited Create(True);
  FFiles := aFiles;
  FEvent := TEvent.Create(nil, true, False, '');
end;

procedure TFilesIndexingThread.Execute;
const
  Timeout = 5000; //Check every 5 sec
var
  Ticks, BeginTickCount: cardinal;
begin
  inherited;
  IndexFiles;

  Ticks := 0;
  while True do begin
    if Terminated
    then Break;
    If (Ticks < Timeout) then FEvent.WaitFor(Timeout - Ticks);
    if Terminated
    then Break;

    BeginTickCount := GetTickCount;

    IndexFiles;

    Ticks := GetTickInterval(BeginTickCount, GetTickCount);
  end;
end;

function TFilesIndexingThread.GetTickInterval(StartTickCount,
  FinishTickCount: Cardinal): Cardinal;
begin
  // each 49.7 days ticks are reseted, so we should take it into attention
  // and use GetTickInterval for avoiding the out of range error
  if FinishTickCount >= StartTickCount then
    Result := FinishTickCount - StartTickCount
  else
    Result := Longword($FFFFFFFF) - StartTickCount + FinishTickCount + 1;
end;

procedure TFilesIndexingThread.IndexFiles;
begin
  var lProject := GetActiveProject;
  if not Assigned(lProject) then //Should only try and find files, if a project is currently active. Exit early otherwise
    Exit;

  var lProjectPath := ExtractFilePath(lProject.FileName);

  var lFiles := TDictionary<string, string>.Create;

  var lTmpFiles := TStringList.Create;
  lProject.GetCompleteFileList(lTmpFiles);

  for var lFile in lTmpFiles do
  begin
    if ExtractFileExt(lFile) = '.pas' then
    begin
      var lRelativePath := ExtractRelativePath(lProjectPath, lFile);
      if not lFiles.ContainsKey(lRelativePath) then
        lFiles.Add(lRelativePath, lFile);
    end;
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
      for var lFile in TDirectory.GetFiles(lPath, '*.pas') do
      begin
        var lRelativePath := ExtractRelativePath(lProjectPath, lFile);
        if not lFiles.ContainsKey(lRelativePath) then
          lFiles.Add(lRelativePath, lFile);
      end;
    end;
  end;

  var lThreadList := FFiles.LockList;
  try
    for var lFilePair in lFiles do
    begin
      var lFileObj := TFileObj.Create(lFilePair.Value, lFilePair.Key);
      lThreadList.Add(lFileObj);
    end;
  finally
    FFiles.UnlockList;
  end;

  FreeAndNil(lFiles);
end;

end.

