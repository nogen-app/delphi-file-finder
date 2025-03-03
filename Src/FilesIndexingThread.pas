unit FilesIndexingThread;

interface

uses
  ToolsAPI,
  System.SysUtils,
  System.Classes, System.SyncObjs, System.Generics.Collections;

type
  TFilesIndexingThread = class(TThread)
    private
      FEvent: TEvent;
      FCallback: TProc<TList<string>>;

      procedure IndexFiles;

      function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
    protected
      procedure Execute; override;
    public
      constructor Create(aDoneIndexingCallback: TProc<TList<string>>);
  end;

implementation

uses
  System.IOUtils;

{ TFilesIndexingThread }

constructor TFilesIndexingThread.Create(aDoneIndexingCallback: TProc<TList<string>>);
begin
  inherited Create(True);
  FCallback := aDoneIndexingCallback;

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

  var lFiles := TList<string>.Create;

  var lTmpFiles := TStringList.Create;
  lProject.GetCompleteFileList(lTmpFiles);

  for var lFile in lTmpFiles do
  begin
    if ExtractFileExt(lFile) = '.pas' then
    begin
      var lRelativePath := ExtractRelativePath(lProjectPath, lFile);
      if not lFiles.Contains(lRelativePath) then
        lFiles.Add(lRelativePath);
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
        if not lFiles.Contains(lRelativePath) then
          lFiles.Add(lRelativePath);
      end;
    end;
  end;

  if Assigned(FCallback) then
  begin
    Synchronize(procedure
    begin
      FCallback(lFiles);
      FreeAndNil(lFiles);
    end);
  end;
end;

end.

