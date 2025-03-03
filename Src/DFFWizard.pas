unit DFFWizard;

interface

uses
  Classes, System.SysUtils, ToolsAPI, Vcl.AppEvnts,
  Vcl.Forms, Winapi.Windows, Winapi.Messages,
  System.Generics.Collections, DFFFilesForm,
  FileIndexThread, Spring.COntainer, ServiceRegistration;

type

  TDFFWizard = class(TNotifierObject, IOTAWizard, IOTAKeyboardBinding)
  private
    FForm: TfrmDFFFiles;
    FIndexThread: TFileIndexThread;

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
    destructor Destroy; override;

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
  //TODO: Should add a menu item, with a settings wheel, so people can change the hotkey

procedure TDFFWizard.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([TextToShortCut('Ctrl+T')], ShowForm, nil, kfImplicitShift, '', '');
end;

constructor TDFFWizard.Create;
begin
  RegisterServices;
  FIndexThread := GlobalContainer.Resolve<TFileIndexThread>;


//  FFilesIndexingThread := TFilesIndexingThread.Create(DoNewFilesIndexed);
//  FFilesIndexingThread.Start;
end;

destructor TDFFWizard.Destroy;
begin
  FIndexThread.Terminate;
  FreeAndNil(FIndexThread);
  inherited;
end;

procedure TDFFWizard.DoNewFilesIndexed(aFiles: TList<string>);
begin
//  if Assigned(FForm) then
//    FForm.Frame.SetFiles(aFiles);
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
  Result := 'nogen, Delphi File finder';
end;

function TDFFWizard.GetIDString: string;
begin
  Result := 'nogen.DelphiFileFinder';
end;

function TDFFWizard.GetName: string;
begin
  Result := 'nogen.DFF';
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
      FForm := TfrmDFFFiles.Create(nil);

    lNTAServices.CreateDockableForm(FForm);
  end;

  BindingResult := TKeyBindingResult.krHandled;
end;

end.
