unit DLLEntry;

interface

uses
  SysUtils, ToolsAPI, DFFWizard;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
exports
  InitWizard name WizardEntryPoint;

implementation

var
  WizardIndex: Integer;

procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if (WizardIndex >= 0) then
  begin
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    WizardServices.RemoveWizard(WizardIndex);
    WizardIndex := 0;
  end;
end;

procedure Registerkeybinds(const BorlandIDEServices: IBorlandIDEServices; aDFFWiz: TDFFWizard);
begin
  var keyboardBinding: IOTAKeyboardServices;
  if Supports(BorlandIDEServices, IOTAKeyboardServices, keyboardBinding) then
  begin
    keyboardBinding.AddKeyboardBinding(aDFFWiz);
  end;
end;


function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Result := BorlandIDEServices <> nil;
  if Result then
  begin
    Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);
    Terminate := FinalizeWizard;
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    DFFWiz := TDFFWizard.Create;

    RegisterKeyBinds(BorlandIDEServices, DFFWiz);

    WizardIndex := WizardServices.AddWizard(DFFWiz as IOTAWizard);
    Result := (WizardIndex >= 0);
  end;
end;

end.
