unit DFFWizard;

interface

uses
  Classes, System.SysUtils, ToolsAPI, Vcl.AppEvnts,
  Vcl.Forms, Winapi.Windows, Winapi.Messages;

type

  TDFFWizard = class(TNotifierObject, IOTAWizard, IOTAKeyboardBinding)
  private
    function GetBindingType: TBindingType;
    function GetDisplayName: string;

    procedure ShowForm(const Context: IOTAKeyContext; KeyCode: TShortcut;
    var BindingResult: TKeyBindingResult);
  public
    constructor Create;

    function GetIDString:string;
    function GetName: string;
    function GetState: TWizardState;

    procedure Execute;

    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    property BindingType: TBindingType read GetBindingType;
    property DisplayName: string read GetDisplayName;
  end;

var
  DFFWiz: TDFFWizard;

implementation

uses
  DFFFilesForm, System.IOUtils,
  Vcl.Controls, Vcl.Menus, Helpers;

{ TDFFWizard }

procedure TDFFWizard.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('T'), [ssCtrl])], ShowForm, nil, 0, '', 'Nogen file finder');
end;

constructor TDFFWizard.Create;
begin
end;

procedure TDFFWizard.Execute;
begin

end;

function TDFFWizard.GetBindingType: TBindingType;
begin
  Result := TBindingType.btPartial;
end;

function TDFFWizard.GetDisplayName: string;
begin
  Result := GetName;
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

procedure TDFFWizard.ShowForm(const Context: IOTAKeyContext; KeyCode: TShortcut;
    var BindingResult: TKeyBindingResult);
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

  var lNTAServices: INTAServices;
  if Supports(BorlandIDEServices, INTAServices, lNTAServices) then
  begin
    var lForm := TfrmDFFFiles.Create(nil);
    lNTAServices.CreateDockableForm(lForm);
    lForm.Frame.SetFiles(lFiles);
  end;
end;

end.
