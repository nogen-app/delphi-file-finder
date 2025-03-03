unit FileIndexThread;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  Services.IRepo;

type
  TFileIndexThread = Class(TThread)
    strict private
      FEvent: TEvent;
      FRepo: IRepo;

      procedure IndexFiles;
    protected
      procedure Execute; override;
    public
      constructor Create(const aRepo: IRepo); overload;
      destructor Destroy; override;
  End;


implementation

uses
  ToolsAPI,
  System.Generics.Collections,
  System.IOUtils,
  Spring.Collections;

{ TFileIndexThread }

constructor TFileIndexThread.Create(const aRepo: IRepo);
begin
  Inherited Create(true);
  FRepo := aRepo;

  Suspended := false;
end;

destructor TFileIndexThread.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

procedure TFileIndexThread.Execute;
begin
  inherited;

  FEvent := TEvent.Create(nil, True, False, '');
  while True do begin

    IndexFiles;


    if Terminated then Break;
    try FEvent.WaitFor(3000); except end; //Wait for 3 seconds
    if Terminated then Break;
  end;
end;


procedure TFileIndexThread.IndexFiles;
begin
  var lProject := GetActiveProject;
  if not Assigned(lProject) then //Should only try and find files, if a project is currently active. Exit early otherwise
    Exit;

  var lProjectPath := ExtractFilePath(lProject.FileName);

  var lFiles := TCollections.CreateList<string>;

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


  FRepo.BatchAddFiles(lFiles);
end;


end.
