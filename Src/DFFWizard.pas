unit DFFWizard;

interface

uses
  Classes, System.SysUtils, ToolsAPI, Vcl.AppEvnts,
  Vcl.Forms, Winapi.Windows, Winapi.Messages;


type



  TDFFWizard = class(TNotifierObject, IOTAWizard)
  private
    FEvents: TApplicationEvents;

    procedure DoApplicationMessage(var Msg: TMsg; var Handled: Boolean);

  protected
    procedure EditKeyUp(Key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
  public
    constructor Create;

    function GetIDString:string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  end;

var
  DFFWiz: TDFFWizard;

implementation

uses
  DFFFilesForm, System.IOUtils,
  Vcl.Controls;

{ TDFFWizard }

constructor TDFFWizard.Create;
begin
  FEvents := TApplicationEvents.Create(nil);
  FEvents.OnMessage := DoApplicationMessage;
end;

procedure TDFFWizard.DoApplicationMessage(var Msg: TMsg; var Handled: Boolean);
var
  Key: Word;
  ScanCode: Word;
  Shift: TShiftState;
begin
  if ((Msg.message = WM_KEYDOWN) or (Msg.message = WM_KEYUP)) then
  begin
    Key := Msg.wParam;
    ScanCode := (Msg.lParam and $00FF0000) shr 16;
    Shift := KeyDataToShiftState(Msg.lParam);

    if Key = VK_PROCESSKEY then
    begin
      Key := MapVirtualKey(ScanCode, 1);
    end;

    if Msg.message = WM_KEYUP then
    begin
      EditKeyUp(Key, ScanCode, Shift, Msg, Handled);
    end;
  end;
end;

procedure TDFFWizard.EditKeyUp(Key, ScanCode: Word; Shift: TShiftState;
  Msg: TMsg; var Handled: Boolean);
begin
  var lCtrl:Boolean := (GetKeyState(VK_CONTROL) < 0);
  if lCtrl and (Chr(Key) = 'T') then
  begin

    //TODO: The finding and ranking of files should be done in a BG thread,
    // so all that needs to happen when they press the button is showing the GUI
    var lProject := GetActiveProject;

    var lFiles := TStringList.Create;
    lFiles.OwnsObjects := True;

    var lTmpFiles := TStringList.Create;
    lProject.GetCompleteFileList(lTmpFiles);

    for var lFile in lTmpFiles do
    begin
      lFiles.AddObject(TPath.GetFileNameWithoutExtension(lFile), TStrObj.Create(lFile));
    end;

    var lSearchUnits := TStringList.Create;
    var lConf := lProject.ProjectOptions as IOTAProjectOptionsConfigurations;

    var lPlatform := lConf.ActiveConfiguration;
    lPlatform.GetValues('DCC_UnitSearchPath', lSearchUnits);

    for var lSearchUnit in lSearchUnits do
    begin
      var lPath :TFileName;
      var lFolder := ExtractFilePath(lProject.FileName);

      lPath := TPath.GetFullPath(TPath.Combine(lFolder, lSearchUnit));

      if TDirectory.Exists(lPath) then
      begin
        for var lFile in TDirectory.GetFiles(lPath, '*.pas') do
        begin
          lFiles.AddObject(TPath.GetFileNameWithoutExtension(lFile), TStrObj.Create(lFile));
        end;
      end;
    end;

    var lForm := TfrmDFFFiles.Create(nil);
    lForm.SetFiles(lFiles);
    if lForm.ShowModal = mrOK then
    begin
      var lActionService :IOTAActionServices;
      if Supports(BorlandIDEServices, IOTAActionServices, lActionService) then
      begin
        lActionService.OpenFile(lForm.SelectedFile);
      end;
    end;
    FreeAndNil(lForm);
    FreeAndNil(lFiles);
    Handled := True;
  end;
end;

procedure TDFFWizard.Execute;
begin

end;

function TDFFWizard.GetIDString: string;
begin
  Result := 'nogen.DelphiFileFinder';
end;

function TDFFWizard.GetName: string;
begin
  Result := 'Nogen wizard';
end;

function TDFFWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
