unit DFFWizard;

interface

uses
  Classes, System.SysUtils, ToolsAPI, Vcl.AppEvnts,
  Vcl.Forms, Winapi.Windows, Winapi.Messages, FilesIndexingThread,
  System.Generics.Collections, DFFFilesForm;

type

  TDFFWizard = class(TNotifierObject, IOTAWizard, IOTAKeyboardBinding)
  private
    FFilesIndexingThread: TFilesIndexingThread;
    FForm: TfrmDFFFiles;

    procedure DoNewFilesIndexed(aFiles: TList<string>);

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
  System.IOUtils,
  Vcl.Controls, Vcl.Menus;

{ TDFFWizard }

  //TODO: Should register the frame, so it can be saved in the layout
  //TODO: Should add a menu item, with a settings wheel, so people can change the hotkey

procedure TDFFWizard.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('T'), [ssCtrl])], ShowForm, nil, 0, '', 'Nogen file finder');
end;

constructor TDFFWizard.Create;
begin
  FFilesIndexingThread := TFilesIndexingThread.Create(DoNewFilesIndexed);
  FFilesIndexingThread.Start;
end;

procedure TDFFWizard.DoNewFilesIndexed(aFiles: TList<string>);
begin
  if Assigned(FForm) then
    FForm.Frame.SetFiles(aFiles);
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
  //TODO: Set the frame in focus, if the hotkey is pressed, while frame is active

  var lNTAServices: INTAServices;
  if Supports(BorlandIDEServices, INTAServices, lNTAServices) then
  begin
    if not Assigned(FForm) then
    begin
      FForm := TfrmDFFFiles.Create(nil);
      lNTAServices.CreateDockableForm(FForm);
    end;
  end;
end;

end.
