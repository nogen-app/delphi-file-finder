unit DFFWizard;

interface

uses
  Classes, System.SysUtils, ToolsAPI, Vcl.AppEvnts,
  Vcl.Forms, Winapi.Windows, Winapi.Messages, FilesIndexingThread,
  System.Generics.Collections, Helpers;

type

  TDFFWizard = class(TNotifierObject, IOTAWizard, IOTAKeyboardBinding)
  private
    FFiles: TThreadList<TFileObj>;
    FFilesIndexingThread: TFilesIndexingThread;


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
  Vcl.Controls, Vcl.Menus;

{ TDFFWizard }

procedure TDFFWizard.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('T'), [ssCtrl])], ShowForm, nil, 0, '', 'Nogen file finder');
end;

constructor TDFFWizard.Create;
begin
  FFiles := TThreadList<TFileObj>.Create;
  FFiles.Duplicates := TDuplicates.dupIgnore;

  FFilesIndexingThread := TFilesIndexingThread.Create(FFiles);
  FFilesIndexingThread.Start;
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
  //TODO:
  //1. Fine tune the fuzzy files finder

  var lNTAServices: INTAServices;
  if Supports(BorlandIDEServices, INTAServices, lNTAServices) then
  begin
    var lForm := TfrmDFFFiles.Create(nil);
    lNTAServices.CreateDockableForm(lForm);
    lForm.Frame.SetFiles(FFiles);
  end;
end;

end.
